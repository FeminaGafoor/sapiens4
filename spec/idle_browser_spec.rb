require_relative 'support/desktop'
require_relative 'support/proxy'

RSpec.describe 'Workspace update subscription', :desktop do
  it 'stays idle, receives changes, and resubscribes after minimizing with its draft intact' do
    skip 'macOS window minimization contract' unless RUBY_PLATFORM.include?('darwin')
    @host = desktop_host
    requests = Queue.new
    hold, held, release = Queue.new, Queue.new, Queue.new
    proxy = HostProxy.new(host, updates: true) do |request, response|
      if request.path.end_with?('/api/state')
        requests << true
        if hold.pop(timeout: 0)
          held << true
          Timeout.timeout(10) { release.pop }
        end
      end
      response
    end
    start_desktop(proxy.url)
    desktop.see('Message ChiefSapi')
    desktop.fill('Message ChiefSapi', 'Keep this unsent draft')
    Contract.wait { requests.size >= 2 }
    sleep 2
    idle = requests.size
    sleep 3
    expect(requests.size).to eq(idle)
    hold << true
    host.agent('FirstUpdate')
    Timeout.timeout(10) { held.pop }
    host.agent('SecondUpdate')
    release << true
    desktop.see('SecondUpdate')
    expect(requests.size).to be > idle
    minimize = desktop.tree.find { |node| node['subrole'] == 'AXMinimizeButton' }
    raise 'Fixture window has no minimize control' unless minimize
    desktop.click_node(minimize)
    # Allow the already in-flight refresh and visibility event to settle.
    sleep 2
    stopped = requests.size
    host.agent('WhileHidden')
    sleep 3
    expect(requests.size).to eq(stopped)
    # Native fixture menu action restores the window; no JS/visibility injection.
    desktop.control('activate')
    desktop.key('command+1')
    Contract.wait { requests.size > stopped }
    desktop.see('WhileHidden')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep this unsent draft')
    sleep 2
    count = requests.size
    sleep 3
    expect(requests.size).to eq(count)

    host.restart
    host.agent('AfterRestart')
    desktop.see('AfterRestart')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep this unsent draft')

    host.control(chief, 'workspace_memo')
    desktop.see('Notes.html')
    notes = File.join(host.root, 'workspaces', chief, 'Notes.html')
    File.open(notes, 'a') { |file| file.puts('<p>Changed directly on disk</p>') }
    desktop.see('Changed directly on disk')
  ensure
    release << true if release
    # Close the UI before removing its proxy.
    @desktop&.close
    @desktop = nil
    proxy&.close
  end
end
