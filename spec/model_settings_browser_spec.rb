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
    desktop.see('Recommended for Claude')
    desktop.press('Customize default model')
    desktop.fill('Default model', 'example-model')
    desktop.press('Default effort High')
    desktop.click('Customize a Sapi')
    desktop.see('Inherits Claude and the corpora model settings.')
    desktop.press('Use corpora default')
    desktop.press('Sapi provider Codex')
    desktop.see('Recommended for Codex')
    desktop.press('Use corpora default')
    desktop.see('Inherits Claude and the corpora model settings.')

    desktop.click('Close preview')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep my unsent message')
    expect(host.state.fetch('orchestration').transform_values { |entry| entry.fetch('execution') }).to eq(original)
    open_models
    desktop.see('Recommended for Codex')
    expect(desktop.text).not_to include('example-model')
    desktop.press('Close dialog')
  end
end
