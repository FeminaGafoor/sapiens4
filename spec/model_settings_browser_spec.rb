require_relative 'support/desktop'

RSpec.describe 'Model settings', :desktop do
  def open_models
    desktop.click('Menu')
    desktop.press('Workspace & settings')
    desktop.press('Models')
    desktop.see('Choose a default model and customize individual Sapis.')
  end

  it 'previews provider and Sapi choices without changing execution settings or the chat draft' do
    start_desktop
    original = host.state.fetch('orchestration').transform_values { |entry| entry.fetch('execution') }
    desktop.fill('Message ChiefSapi', 'Keep my unsent message')
    open_models
    desktop.see('Applies to new runs. Running work keeps its settings. Model and effort selections replace Normal/Deep mode.')
    expect(desktop.find('Save and apply', role: 'AXButton')['enabled']).to eq(false)

    desktop.press('Default provider Claude')
    desktop.see('Sapiens4 recommendation: Opus 5.5 · medium')
    desktop.press('Sapi customization')
    desktop.see('Inherits Claude and the corpora model settings.')
    desktop.press('Use corpora default')
    desktop.press('Sapi provider Codex')
    desktop.see('Sapiens4 recommendation: Astra · medium')
    desktop.press('Customize sapi model')
    desktop.find('Sapi model')
    desktop.find('Sapi effort')
    desktop.press('Use corpora default')
    desktop.see('Inherits Claude and the corpora model settings.')
    desktop.press('Global settings')
    desktop.see('Sapiens4 recommendation: Opus 5.5 · medium')
    desktop.press('Sapi customization')
    desktop.see('Inherits Claude and the corpora model settings.')

    desktop.click('Close')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep my unsent message')
    expect(host.state.fetch('orchestration').transform_values { |entry| entry.fetch('execution') }).to eq(original)
    open_models
    desktop.see('Sapiens4 recommendation: Astra · medium')
    expect(desktop.text).not_to include('Provider model name')
    desktop.press('Close dialog')
  end
  it 'shows a discovery failure and retries without closing the settings' do
    start_desktop
    host.configure(catalog_error: true)
    open_models
    desktop.see('Retry model discovery')
    host.configure(catalog_error: false)
    desktop.click('Retry model discovery')
    desktop.see('Sapiens4 recommendation: Astra · medium')
    desktop.press('Customize default model')
    desktop.find('Default model')
    desktop.find('Default effort')
    desktop.click('Close')
  end
  it 'resets the effort when switching to a model with fewer supported choices' do
    start_desktop
    host.configure(catalog_pages: [{data: [
      {model: 'fixture-general', displayName: 'Fixture General', hidden: false, isDefault: true,
       defaultReasoningEffort: 'low', supportedReasoningEfforts: [{reasoningEffort: 'low'}, {reasoningEffort: 'ultra'}]},
      {model: 'fixture-quick', displayName: 'Fixture Quick', hidden: false, isDefault: false,
       defaultReasoningEffort: 'low', supportedReasoningEfforts: [{reasoningEffort: 'low'}]}
    ]}])
    open_models
    desktop.see('Sapiens4 recommendation: Astra · medium')
    desktop.press('Customize default model')
    desktop.press('Default effort')
    desktop.key('end')
    desktop.key('return')
    desktop.value('Default effort', 'ultra')
    desktop.press('Default model')
    desktop.key('end')
    desktop.key('return')
    desktop.value('Default model', 'Fixture Quick')
    desktop.value('Default effort', 'Model default (low)')
    desktop.press('Default effort')
    expect(desktop.text).not_to include('ultra')
    desktop.key('escape')
    desktop.click('Close')
  end

  it 'applies a custom model and restores it on reopening' do
    start_desktop
    original = host.state.fetch('orchestration')
    open_models
    desktop.see('Sapiens4 recommendation: Astra · medium')
    desktop.press('Customize default model')
    desktop.value('Default model', 'Fixture General · Codex default')
    desktop.click('Save and apply')
    desktop.see('Model settings applied to new runs.')
    expect(desktop.find('Save and apply', role: 'AXButton')['enabled']).to eq(false)
    desktop.click('Close')
    open_models
    desktop.value('Default model', 'Fixture General · Codex default')
    desktop.value('Default effort', 'high')
    expect(host.state.fetch('orchestration')).to eq(original)
    expect(host.api('GET', '/api/model-settings').fetch('defaults')).to include('model' => 'fixture-general', 'effort' => 'high')
    desktop.click('Close')
    host.turn(host.submit(chief, 'Use the model selected in settings'))
    expect(host.calls).not_to be_empty
    host.calls.each do |call|
      expect(call.fetch('args')).to include('model="fixture-general"', 'model_reasoning_effort="high"')
    end
  end

  it 'keeps unsaved selections when a concurrent save causes a revision conflict' do
    start_desktop
    open_models
    desktop.see('Sapiens4 recommendation: Astra · medium')
    desktop.press('Customize default model')
    host.api('PUT', '/api/model-settings', {
      revision: 0, defaults: {provider: 'codex', model: 'fixture-general', effort: 'high'}, overrides: {}})
    desktop.click('Save and apply')
    desktop.see('Model settings changed elsewhere. Close and reopen Models before saving.')
    desktop.value('Default model', 'Fixture General · Codex default')
    expect(host.api('GET', '/api/model-settings').fetch('revision')).to eq(1)
    desktop.click('Close')
  end

  it 'applies the Claude recommendation and restores it on reopening' do
    start_desktop
    open_models
    desktop.press('Default provider Claude')
    desktop.see('Sapiens4 recommendation: Opus 5.5 · medium')
    desktop.click('Save and apply')
    desktop.see('Model settings applied to new runs.')
    desktop.click('Close')
    open_models
    desktop.see('Sapiens4 recommendation: Opus 5.5 · medium')
    desktop.click('Close')
    host.turn(host.submit(chief))
    expect(host.calls).not_to be_empty
    host.calls.each do |call|
      expect(call.fetch('provider')).to eq('claude')
      expect(call.fetch('args')).to include('--model', 'claude-opus-5-5', '--effort', 'medium')
    end
  end

  it 'clears and disables effort when switching to a Claude model without effort support' do
    start_desktop
    open_models
    desktop.press('Default provider Claude')
    desktop.see('Sapiens4 recommendation: Opus 5.5 · medium')
    desktop.press('Customize default model')
    desktop.value('Default effort', 'medium')
    desktop.press('Default model')
    desktop.key('end')
    desktop.key('return')
    desktop.value('Default model', 'Fixture no effort')
    desktop.value('Default effort', 'Provider default')
    expect(desktop.find('Default effort', role: 'AXPopUpButton')['enabled']).to eq(false)
    desktop.click('Save and apply')
    desktop.see('Model settings applied to new runs.')
    desktop.click('Close')
    open_models
    desktop.value('Default effort', 'Provider default')
    expect(desktop.find('Default effort', role: 'AXPopUpButton')['enabled']).to eq(false)
    desktop.click('Close')
  end

  it 'preserves changes across pages and saves global settings and a Sapi override together' do
    start_desktop
    open_models
    original = host.api('GET', '/api/model-settings')
    desktop.press('Default provider Claude')
    desktop.press('Sapi customization')
    expect(desktop.text).not_to include('Default provider Codex')
    desktop.press('Use corpora default')
    desktop.press('Sapi provider Codex')
    desktop.press('Customize sapi model')
    desktop.value('Sapi model', 'Fixture General · Codex default')
    desktop.press('Global settings')
    desktop.see('Sapiens4 recommendation: Opus 5.5 · medium')
    expect(host.api('GET', '/api/model-settings')).to eq(original)
    desktop.press('Sapi customization')
    desktop.value('Sapi model', 'Fixture General · Codex default')
    desktop.value('Sapi effort', 'Model default (high)')
    desktop.click('Save and apply')
    desktop.see('Model settings applied to new runs.')
    saved = host.api('GET', '/api/model-settings')
    expect(saved.fetch('defaults')).to include('provider' => 'claude')
    expect(saved.fetch('overrides').fetch(chief)).to include('provider' => 'codex', 'model' => 'fixture-general', 'effort' => 'high')
    desktop.click('Close')
    open_models
    desktop.see('Sapiens4 recommendation: Opus 5.5 · medium')
    desktop.press('Sapi customization')
    desktop.value('Sapi model', 'Fixture General · Codex default')
    desktop.value('Sapi effort', 'high')
    desktop.press('Global settings')
    expect(desktop.find('Save and apply', role: 'AXButton')['enabled']).to eq(false)
    desktop.click('Close')
  end

  it 'explains why an invalid override blocks saving from the global page' do
    start_desktop
    open_models
    desktop.press('Default provider Claude')
    desktop.see('Unsaved changes. Save and apply saves both pages.')
    desktop.press('Sapi customization')
    desktop.press('Use corpora default')
    desktop.press('Sapi provider Codex')
    desktop.press('Global settings')
    desktop.see('Cannot save yet. Check model availability and effort for: Sapi customization — ChiefSapi.')
    expect(desktop.find('Save and apply', role: 'AXButton')['enabled']).to eq(false)
    desktop.press('Sapi customization')
    desktop.press('Customize sapi model')
    desktop.value('Sapi model', 'Fixture General · Codex default')
    desktop.press('Global settings')
    desktop.see('Unsaved changes. Save and apply saves both pages.')
    desktop.click('Save and apply')
    desktop.see('Model settings applied to new runs.')
    desktop.click('Close')
    open_models
    desktop.see('Model settings applied to new runs.')
    expect(desktop.find('Save and apply', role: 'AXButton')['enabled']).to eq(false)
    desktop.click('Close')
  end

end
