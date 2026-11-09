require_relative 'support/desktop'

RSpec.describe 'Linux computer settings', :desktop do
  before { skip 'Linux settings' unless Desktop.linux? }

  it 'shows native accessibility diagnostics and Linux permission guidance' do
    start_desktop
    desktop.click('Open shared computer')
    desktop.see('AT-SPI accessibility is available')
    expect(desktop.text).to include('GNOME foreground extension', 'Xorg uses native window ownership checks')
    expect(desktop.text).not_to include('macOS Accessibility', 'Windows UI Automation')
  end
end
