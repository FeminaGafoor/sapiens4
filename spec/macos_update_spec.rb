require_relative 'support/desktop'
require 'securerandom'
require 'digest'
require 'etc'

# Detached GUI children retain the Coordinator's stderr; file capture permits
# waiting for the short-lived Coordinator without waiting for the GUI to quit.
class MacUpdateChild
  attr_reader :pid, :input
  def initialize(*args, env:, log:)
    @log = log
    reader, @input = IO.pipe
    @pid = Process.spawn(env, *args, in: reader, out: log, err: [:child, :out])
    reader.close
  end
  def stdout; File.exist?(@log) ? File.read(@log) : ''; end
  alias stderr stdout
  def finish(seconds = 30)
    @status ||= Timeout.timeout(seconds) { Process.wait2(pid).last }
    [stdout, stderr, @status.exitstatus]
  end
  def signal(name)
    Process.kill(name, pid) unless @status
  rescue Errno::ESRCH
  end
  def close
    input.close unless input.closed?
    signal('KILL')
    finish
  end
end

class MacUpdateDesktop < Desktop
  def initialize(**options)
    @process = options.fetch(:process)
    control('activate')
    super(**options)
  end
  def control(command, *args)
    return super unless %w[activate key click paste set-value press].include?(command)
    Contract.wait(timeout: 10, description: "fixture focus before #{command}") do
      begin
        super('activate') unless command == 'activate'
        super(command, *args)
      rescue RuntimeError => error
        # These driver rejections occur before input is sent. Never repeat an
        # action after an ambiguous result or a successful injection.
        raise unless error.message.match?(/Target application is not frontmost|Click point belongs to another process/)
        false
      end
    end
  end
end

RSpec.describe 'macOS D2 installed update', :macos do
  def command(*args)
    out, err, status = Contract.capture(*args, timeout: 120)
    expect(status).to eq(0), "#{args.inspect}: #{out}#{err}"
    out
  end

  def decision
    JSON.parse(File.read(File.join(@installation, 'update-state.json')))
  end

  def executable(version)
    File.join(@installation, 'versions', version, 'Sapiens4.app/Contents/MacOS/Sapiens4')
  end

  def running(version)
    path = executable(version)
    IO.popen(['ps', '-axo', 'pid=,stat=,comm='], &:read).lines.filter_map do |line|
      pid, status, name = line.strip.split(/\s+/, 3)
      pid.to_i if name == path && !status.include?('Z')
    end
  end

  def desktop(version)
    pid = Contract.wait(timeout: 45, description: "LaunchServices registers native #{version}") do
      pids = running(version)
      app = JSON.parse(command(Desktop.driver, 'apps')).fetch('apps').find { |item| pids.include?(item['pid']) }
      if app
        expect(app['bundleId']).to eq('com.sapiens4.desktop')
        app['pid']
      end
    end
    ui = Contract.wait(timeout: 10, description: 'selected native app becomes frontmost') do
      begin
        MacUpdateDesktop.new(process: Struct.new(:pid).new(pid), ready: false)
      rescue RuntimeError => error
        # Activation is idempotent; no user input has been sent on this path.
        raise unless error.message.include?('Target application is not frontmost')
        false
      end
    end
    ui.see('Message ChiefSapi')
    ui
  end

  def quit(ui)
    ui.key('command+q')
    Contract.wait(timeout: 30, description: 'native application and backend shutdown') do
      !IO.popen(['ps', '-axo', 'pid=,stat=,comm='], &:read).lines.any? do |line|
        pid, status, = line.strip.split(/\s+/, 3)
        pid.to_i == ui.process.pid && !status.include?('Z')
      end
    end
  end

  def start(*args)
    # A Starter exits while its selected GUI continues. File logs avoid waiting
    # for the inherited stdout pipe of that intentionally detached GUI.
    pid = Process.spawn(@env, @starter, *args, out: @log, err: [:child, :out])
    status = Timeout.timeout(45) { Process.wait2(pid).last }
    expect(status.success?).to be(true), File.read(@log)
  end

  def stage(from)
    version = SecureRandom.uuid
    destination = File.dirname(File.dirname(File.dirname(executable(version))))
    FileUtils.mkdir_p(File.dirname(destination))
    command('ditto', File.dirname(File.dirname(File.dirname(executable(from)))), destination)
    command('codesign', '--verify', '--deep', '--strict', destination)
    File.write(File.join(File.dirname(destination), 'sapiens-release.version'), "0.2.0\n")
    version
  end

  def coordinator(owner, target)
    child = MacUpdateChild.new(executable(owner), '--d2-update', target, @provider.root,
      @provider.environment.fetch('SAPIENS_CONFIG_DIR'), env: @env, log: File.join(@directory, target + '.log'))
    @coordinators << child
    Contract.wait(description: 'temporary Coordinator handshake') { child.stdout.include?('D2: coordinator') }
    child.input.puts('handoff'); child.input.flush
    child
  end

  def native_update(ui, owner)
    tree = ui.control('tree', '--depth', '8').fetch('tree')
    find = lambda do |node, path|
      next path if node['title'] == 'Update & Restart' && node['role'] == 'AXMenuItem'
      node.fetch('children', []).each_with_index.filter_map do |child, index|
        find.call(child, path.empty? ? index.to_s : "#{path}.#{index}")
      end.first
    end
    path = find.call(tree, '')
    expect(path).not_to be_nil
    ui.control('press', '--path', path)
    next_version = Contract.wait(timeout: 90, description: 'native Update & Restart commits') do
      state = decision
      selected = state.dig('selection', 'current')
      selected if state['state'] == 'Settled' && selected != owner
    end
    next_ui = desktop(next_version)
    Contract.wait(description: 'old desktop and temporary Coordinator exit') { running(owner).empty? }
    [next_version, next_ui]
  end

  def committed(child, version)
    Contract.wait(timeout: 60, description: "durable selection #{version}: #{child.stderr}") do
      state = decision
      state['state'] == 'Settled' && state.dig('selection', 'current') == version
    end
    ui = desktop(version)
    expect(child.finish(30).last).to eq(0), child.stderr
    ui
  end

  around do |example|
    skip 'Set SAPIENS_TEST_D2_MAC_DMG to the local macOS D2 image' unless RUBY_PLATFORM.include?('darwin') && ENV['SAPIENS_TEST_D2_MAC_DMG']
    @provider = Host.new
    @provider.stop
    @coordinators = []
    Dir.mktmpdir('sapiens-d2-mac-') do |temporary|
      @directory = File.realpath(temporary)
      @installation = File.join(@directory, 'installation')
      @log = File.join(@directory, 'native.log')
      @env = @provider.environment.merge('SAPIENS_DATA_DIR' => @provider.root,
        'SAPIENS_INSTALL_DIR' => @installation, 'SAPIENS_D2_FIXTURE_PAYLOAD' => ENV['SAPIENS_TEST_D2_MAC_PAYLOAD'], 'SAPIENS_REPOSITORY' => nil, 'SAPIENS_RUNTIME_DIR' => nil)
      mount = File.join(@directory, 'volume')
      FileUtils.mkdir_p(mount)
      command('hdiutil', 'attach', '-readonly', '-nobrowse', '-mountpoint', mount, File.realpath(ENV.fetch('SAPIENS_TEST_D2_MAC_DMG')))
      begin
        command('ditto', File.join(mount, 'Sapiens4.app'), File.join(@directory, 'Sapiens4.app'))
      ensure
        command('hdiutil', 'detach', mount)
      end
      @starter = File.join(@directory, 'Sapiens4.app/Contents/MacOS/Sapiens4')
      example.run
    ensure
      evidence = File.join(Contract::ROOT, '.build/d2-evidence', File.basename(@directory))
      FileUtils.mkdir_p(evidence)
      Dir[File.join(@directory, '*.log')].each { |file| FileUtils.cp(file, evidence) }
      if example.exception
        File.write(File.join(evidence, 'processes.txt'), IO.popen(['ps', '-axo', 'pid=,ppid=,stat=,command='], &:read).lines.select { |line| line.include?(@installation) }.join)
        config = @provider.environment.fetch('SAPIENS_CONFIG_DIR')
        prefs = File.join(config, 'macos-preferences.plist')
        FileUtils.cp(prefs, evidence) if File.exist?(prefs)
        FileUtils.cp(File.join(@installation, 'update-state.json'), evidence) if File.exist?(File.join(@installation, 'update-state.json'))
        IO.popen(['ps', '-axo', 'pid=,comm='], &:read).lines.each do |line|
          pid, path = line.strip.split(/\s+/, 2)
          if path&.start_with?(@installation + '/') && path.end_with?('/MacOS/Sapiens4')
            result = JSON.parse(IO.popen([Desktop.driver, 'tree', '--pid', pid, '--depth', '12'], &:read))
            result['tree']['children'].select! { |node| node['role'] == 'AXWindow' } if result['tree']
            File.write(File.join(evidence, "tree-#{pid}.json"), JSON.pretty_generate(result))
          end
        end
      end
      @coordinators.each { |child| child.signal('KILL'); child.close }
      IO.popen(['ps', '-axo', 'pid=,comm='], &:read).lines.each do |line|
        pid, path = line.strip.split(/\s+/, 2)
        Process.kill('CONT', pid.to_i) if path&.start_with?(@installation + '/') rescue Errno::ESRCH
        Process.kill('TERM', pid.to_i) if path&.start_with?(@installation + '/') rescue Errno::ESRCH
      end
      # Remove only the suite derived from this temporary Config directory.
      config = @provider.environment.fetch('SAPIENS_CONFIG_DIR')
      suite = 'com.sapiens4.client.' + Digest::SHA256.hexdigest(File.realpath(config))
      system('/usr/bin/defaults', 'delete', suite, out: File::NULL, err: File::NULL)
    end
  ensure
    @provider&.close
  end

  it 'launches the short-lived Starter, commits A→B→C at Workspace Ready, and preserves new work against stale recovery' do
    start
    a = decision.fetch('selection').fetch('current')
    ui = desktop(a)
    ui.fill('Message ChiefSapi', 'Unsent D2 native draft')
    config = @provider.environment.fetch('SAPIENS_CONFIG_DIR')
    prefs = File.join(config, 'macos-preferences.plist')
    Contract.wait(description: 'draft is durable before a clean quit') do
      File.file?(prefs) && command('plutil', '-p', prefs).include?('Unsent D2 native draft')
    end
    quit(ui)
    note = File.join(@provider.root, 'owned-note')
    File.write(note, 'before B')
    File.chmod(0640, note)
    command('xattr', '-w', 'com.sapiens4.d2-test', 'preserved', note)
    command('chmod', '+a', "user:#{Etc.getpwuid.name} allow read,write", note)
    if ENV['SAPIENS_TEST_D2_MAC_PAYLOAD']
      start
      b, ui = native_update(desktop(a), a)
    else
      b = stage(a)
      ui = committed(coordinator(a, b), b)
    end
    expect(ui.value('Message ChiefSapi', 'Unsent D2 native draft')).to be_truthy
    expect(decision.dig('selection', 'previous', 'version')).to eq(a)
    first_backup = decision.dig('selection', 'previous', 'backup')
    if ENV['SAPIENS_TEST_D2_MAC_PAYLOAD']
      c, ui = native_update(ui, b)
    else
      quit(ui)
      c = stage(b)
      ui = committed(coordinator(b, c), c)
    end
    expect(decision.dig('selection', 'previous', 'version')).to eq(b)
    expect(File.exist?(File.join(@installation, 'versions', a))).to be(false)
    expect(File.exist?(File.join(@installation, 'backups', first_backup))).to be(false)
    ui.fill('Message ChiefSapi', 'Committed C work')
    ui.press('Send message')
    ui.see('Contract test reply')
    quit(ui)
    File.write(note, 'new committed C work')
    stale = MacUpdateChild.new(executable(b), '--d2-recover', env: @env, log: File.join(@directory, 'stale.log'))
    @coordinators << stale
    ui = desktop(c)
    expect(ui.see('Contract test reply')).to include('Committed C work')
    expect(File.read(note)).to eq('new committed C work')
    expect(command('xattr', '-p', 'com.sapiens4.d2-test', note).strip).to eq('preserved')
    expect(File.stat(note).mode & 0777).to eq(0640)
    expect(command('ls', '-le', note)).to include('allow read,write')
    # Reopening the stable Starter activates this exact selected process.
    start
    expect(running(c)).to eq([ui.process.pid])
    quit(ui)
    expect(stale.finish(30).last).to eq(0)
  end

  it 'recovers an interrupted target without losing the original backup, metadata, or native preferences' do
    start
    a = decision.fetch('selection').fetch('current')
    ui = desktop(a)
    ui.fill('Message ChiefSapi', 'Draft before interruption')
    quit(ui)
    config = @provider.environment.fetch('SAPIENS_CONFIG_DIR')
    note = File.join(config, 'owned-note')
    File.write(note, 'original')
    command('xattr', '-w', 'com.sapiens4.d2-test', 'original metadata', note)
    b = stage(a)
    child = coordinator(a, b)
    Contract.wait(timeout: 45, description: "Pending transaction: #{child.stderr}") { decision['state'] == 'Pending' }
    candidate = Contract.wait(description: 'candidate PID before Workspace Ready') { running(b).first }
    Process.kill('STOP', candidate)
    expect(decision['state']).to eq('Pending')
    backup = File.join(@installation, 'backups', decision.fetch('attempt'))
    saved_digest = Digest::SHA256.file(File.join(backup, '1', 'owned-note')).hexdigest
    File.write(note, 'partial target mutation')
    command('xattr', '-w', 'com.sapiens4.d2-test', 'partial metadata', note)
    child.signal('KILL')
    Process.kill('CONT', candidate)
    Contract.wait(timeout: 20, description: 'abandoned candidate relinquishes its lease') { running(b).empty? }
    expect(Digest::SHA256.file(File.join(backup, '1', 'owned-note')).hexdigest).to eq(saved_digest)
    start
    ui = desktop(a)
    expect(decision['state']).to eq('Settled')
    expect(File.read(note)).to eq('original')
    expect(command('xattr', '-p', 'com.sapiens4.d2-test', note).strip).to eq('original metadata')
    expect(ui.value('Message ChiefSapi', 'Draft before interruption')).to be_truthy
    expect(File.exist?(File.join(@installation, 'versions', b))).to be(false)
    quit(ui)
  end
end
