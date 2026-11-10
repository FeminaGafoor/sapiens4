require_relative 'spec_helper'

RSpec.describe 'Model settings and account integration', :host do
  it 'connects from settings on a fresh profile without visiting the welcome screen' do
    skip 'The fixture executable uses a POSIX script' if Gem.win_platform?
    host.stop
    config = host.environment.fetch('SAPIENS_CONFIG_DIR')
    FileUtils.rm_f([File.join(config, 'onboarding.json'), File.join(config, 'codex/auth.json')])
    File.write(host.environment.fetch('SAPIENS_CODEX_BINARY'), <<~RUBY)
      #!#{RbConfig.ruby}
      root, home = ENV.fetch('SAPIENS_CONTRACT_ENV'), ENV.fetch('CODEX_HOME')
      if ARGV.include?('logout')
        File.delete(File.join(home, 'auth.json')) if File.exist?(File.join(home, 'auth.json'))
        exit
      end
      if ARGV.include?('login')
        exit(File.exist?(File.join(home, 'auth.json')) ? 0 : 1) if ARGV.include?('status')
        $stdout.sync = true
        if ARGV.include?('--device-auth')
          puts 'Enter this one-time code'
          puts 'ABCD-EFGH'
        else
          warn 'https://auth.openai.com/oauth/authorize?state=synthetic&response_type=code'
        end
        sleep 0.02 until File.exist?(File.join(root, 'finish-login'))
        File.write(File.join(home, 'auth.json'), 'synthetic settings authentication')
        exit
      end
      load #{File.join(__dir__, 'support/provider.rb').inspect}
    RUBY
    host.start
    expect(host.api('GET', '/api/host/onboarding')).to include('completed' => false, 'provider' => 'sign_in_required')
    expect(host.api('POST', '/api/host/onboarding', action: 'login')).to include('provider' => 'signing_in')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'url').to_s.include?('response_type=code') }
    host.api('POST', '/api/host/onboarding', action: 'cancel')
    host.restart
    expect(host.api('GET', '/api/host/onboarding')).to include('completed' => false, 'ready' => false)
    host.api('POST', '/api/host/onboarding', action: 'device-login')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'code') == 'ABCD-EFGH' }
    File.write(File.join(host.root, 'finish-login'), '')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('ready') }
    expect(host.api('GET', '/api/host/onboarding')).to include('completed' => true, 'hold' => '')
    host.api('POST', '/api/host/onboarding', action: 'change-account')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('ready') }
    expect(host.api('GET', '/api/host/onboarding')).to include('hold' => 'review')
    expect(host.api('POST', '/api/host/onboarding', action: 'resume')).to include('hold' => '')
  end

  it 'isolates Codex account holds from Claude, protects active Codex work and preserves model choices' do
    config = host.environment.fetch('SAPIENS_CONFIG_DIR')
    settings = host.api('GET', '/api/model-settings')
    settings['defaults'] = {provider: 'claude', model: 'claude-opus-5-5', effort: 'low'}
    settings['overrides'] = {chief => {provider: 'codex', model: 'fixture-general', effort: 'high'}}
    saved = host.api('PUT', '/api/model-settings', settings)
    researcher = host.agent

    # Profile holds cover Codex only, including admission and scheduled execution.
    Contract.atomic_json(File.join(config, 'account-state.json'), hold: 'review')
    expect(host.request('POST', "/api/agents/#{chief}/messages", {text: 'Wait for account review'}).first).to eq(409)
    host.turn(host.submit(researcher, 'Claude remains available'))
    expect(host.calls).not_to be_empty
    expect(host.calls.map { |call| call.fetch('provider') }.uniq).to eq(['claude'])
    expect(host.api('GET', '/api/model-settings')).to eq(saved)

    Contract.atomic_json(File.join(config, 'account-state.json'), hold: '')
    host.configure(rules: {'Assessing' => {block: true}})
    pending = host.submit(chief, 'Keep the current Codex account')
    Contract.wait { host.calls.any? { |call| call['provider'] == 'codex' } }
    expect(host.request('POST', '/api/host/onboarding', {action: 'change-account'}).first).to eq(409)
    expect(File.read(File.join(config, 'codex/auth.json'))).to eq('synthetic fixture authentication')
    host.release
    host.turn(pending)
    host.idle

    # A Claude-only installation can defer Codex setup and still execute work.
    host.environment['SAPIENS_CODEX_BINARY'] = File.join(host.root, 'missing-codex')
    File.delete(File.join(config, 'codex/auth.json'))
    host.restart
    expect(host.api('POST', '/api/host/onboarding', action: 'defer')).to include('completed' => true)
    count = host.calls.length
    host.turn(host.submit(researcher, 'Continue without Codex installed'))
    expect(host.calls.drop(count)).not_to be_empty
    expect(host.calls.drop(count).map { |call| call.fetch('provider') }.uniq).to eq(['claude'])
    expect(host.api('GET', '/api/model-settings')).to eq(saved)
    expect(host.request('POST', "/api/agents/#{chief}/messages", {text: 'Codex remains unavailable'}).first).to eq(409)
  end
end
