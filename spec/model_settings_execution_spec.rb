require 'yaml'

RSpec.describe 'Applied Codex model settings', :host do
  def choice(model = 'gpt-6-astra', effort = 'medium')
    {provider: 'codex', model: model, effort: effort}
  end

  def settings(revision: 0, defaults: choice, overrides: {})
    {revision: revision, defaults: defaults, overrides: overrides}
  end

  def apply_settings(**fields)
    host.api('PUT', '/api/model-settings', settings(**fields))
  end

  def expect_calls(calls, model, effort)
    expect(calls).not_to be_empty
    calls.each do |call|
      expect(call.fetch('args')).to include("model=\"#{model}\"", "model_reasoning_effort=\"#{effort}\"",
        '--dangerously-bypass-approvals-and-sandbox')
    end
  end

  it 'persists defaults and Sapi overrides and restores inheritance' do
    agent = host.agent
    saved = apply_settings(overrides: {agent => choice('gpt-6-luna', 'low')})
    host.restart
    expect(host.api('GET', '/api/model-settings')).to eq(saved)
    host.turn(host.submit(chief, 'Use the corpora default'))
    host.turn(host.submit(agent, 'Use my override'))
    expect_calls(host.calls.select { |call| call['owner'] == chief }, 'gpt-6-astra', 'medium')
    expect_calls(host.calls.select { |call| call['owner'] == agent }, 'gpt-6-luna', 'low')
    count = host.calls.size
    apply_settings(revision: 1)
    host.turn(host.submit(agent, 'Inherit the corpora default'))
    expect_calls(host.calls.drop(count), 'gpt-6-astra', 'medium')
  end

  {'built-in default settings' => false, 'previously applied settings' => true}.each do |label, already_applied|
    it "keeps #{label} for every step and audit record of a running task while new runs use the update" do
      apply_settings if already_applied
      expected_model = 'gpt-6-astra'
      expected_effort = 'medium'
      host.configure(mode: 'Exec', rules: {'Assessing' => {block: true}})
      first = host.submit(chief, 'Keep this run consistent')
      Contract.wait(description: 'first model invocation') { host.calls.any? }
      run = host.calls.first.fetch('agency_run')
      apply_settings(revision: already_applied ? 1 : 0, defaults: choice('gpt-6-luna', 'low'))
      host.release
      host.turn(first)
      host.idle
      calls = host.calls.select { |call| call['agency_run'] == run }
      expect(calls.map { |call| call['node'] }).to include('Assessing', 'ReadyToWork', 'Execution')
      expect_calls(calls, expected_model, expected_effort)
      tasks = YAML.safe_load(host.api('GET', "/api/agents/#{chief}/tasks")).fetch('tasks')
      records = tasks.flat_map { |task| task.fetch('decisions') }
      expect(records).not_to be_empty
      expect(records).to all(include('model' => expected_model, 'effort' => expected_effort))
      count = host.calls.size
      host.turn(host.submit(chief, 'Use the new settings'))
      expect_calls(host.calls.drop(count), 'gpt-6-luna', 'low')
    end
  end

  {
    'stale revision' => [{revision: 0}, 409],
    'unsupported effort' => [{defaults: {provider: 'codex', model: 'gpt-6-luna', effort: 'ultra'}}, 400],
    'unknown provider' => [{defaults: {provider: 'unknown', model: 'unsupported', effort: 'medium'}}, 400],
    'nonexistent Sapi' => [{overrides: {'missing-sapi' => {provider: 'codex', model: 'gpt-6-astra', effort: 'medium'}}}, 404]
  }.each do |label, (changes, status)|
    it "rejects #{label} without replacing active settings" do
      saved = apply_settings
      expect(host.request('PUT', '/api/model-settings', settings(revision: 1).merge(changes)).first).to eq(status)
      expect(host.api('GET', '/api/model-settings')).to eq(saved)
    end
  end

  it 'reports provider execution failure without silently falling back to another model' do
    apply_settings(defaults: choice('gpt-6-luna', 'low'))
    host.configure(fail: true)
    host.turn(host.submit(chief), 'failed')
    expect_calls(host.calls, 'gpt-6-luna', 'low')
  end
end
