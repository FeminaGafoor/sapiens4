RSpec.describe 'Telemetry availability', :macos do
  before do
    skip 'macOS certificate store contract' unless RUBY_PLATFORM.include?('darwin')
    @host = Host.new(start: false)
    bin = File.join(host.fixture_dir, 'empty-bin')
    FileUtils.mkdir_p(bin)
    host.environment.merge!('PATH' => bin, 'SSL_CERT_FILE' => nil, 'SSL_CERT_DIR' => nil,
      'SAPIENS_TELEMETRY_ORIGIN' => 'https://127.0.0.1:9')
  end

  after { @host&.close }

  it 'starts and stops with telemetry disabled when certificate tools are absent' do
    output, error, code = Contract.capture(Contract::BINARY, '--port', '0', '--data-dir', host.root,
      '--desktop', '--telemetry-consent', 'false', env: host.environment)
    expect(code).to eq(0), output + error
    expect(output).to include('Sapiens4: http://127.0.0.1:')
    expect(JSON.parse(File.read(File.join(host.environment.fetch('SAPIENS_CONFIG_DIR'), 'telemetry.json'))))
      .to include('enabled' => false)
  end

  it 'keeps the service healthy when enabling telemetry fails to initialize TLS' do
    host.start
    expect(host.api('GET', '/api/host/telemetry')).to include('enabled' => false, 'available' => true)
    host.api('PUT', '/api/host/telemetry', enabled: true)
    host.api('POST', '/api/host/telemetry/heartbeat', {ui_visible: true}, status: 202)
    failure = Contract.wait(description: 'telemetry TLS failure') do
      state = host.api('GET', '/api/host/telemetry')
      state unless state.fetch('available')
    end
    expect(failure.fetch('error')).to include('security')
    expect(host.api('GET', '/api/health')).to include('status' => 'ok')
  end
end
