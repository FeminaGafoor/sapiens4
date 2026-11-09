require_relative 'support/desktop'

RSpec.describe 'Previewing model settings', :desktop do
  def open_models
    desktop.click('Menu')
    desktop.press('Workspace & settings')
    desktop.press('Models')
    desktop.see('Set the default provider and model for this corpora.')
  end

  it 'previews provider and Sapi choices without changing execution settings or the chat draft' do
    start_desktop
    original = host.state.fetch('orchestration').transform_values { |entry| entry.fetch('execution') }
    desktop.fill('Message ChiefSapi', 'Keep my unsent message')
    open_models
    desktop.see('Preview only. Changes are not saved.')
    expect(desktop.find('Save changes', role: 'AXButton')['enabled']).to eq(false)

    desktop.press('Default provider Claude')
    desktop.see('Claude model discovery is not connected yet.')
    desktop.click('Customize a Sapi')
    desktop.see('Inherits Claude and the corpora model settings.')
    desktop.press('Use corpora default')
    desktop.press('Sapi provider Codex')
    desktop.see('Fixture General · Codex default effort: high')
    desktop.press('Customize sapi model')
    desktop.find('Sapi model')
    desktop.find('Sapi effort')
    desktop.press('Use corpora default')
    desktop.see('Inherits Claude and the corpora model settings.')

    desktop.click('Close preview')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep my unsent message')
    expect(host.state.fetch('orchestration').transform_values { |entry| entry.fetch('execution') }).to eq(original)
    open_models
    desktop.see('Fixture General · Codex default effort: high')
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
    desktop.see('Fixture General · Codex default effort: high')
    desktop.press('Customize default model')
    desktop.find('Default model')
    desktop.find('Default effort')
    desktop.click('Close preview')
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
    desktop.see('Fixture General · Codex default effort: low')
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
    desktop.click('Close preview')
  end

end
