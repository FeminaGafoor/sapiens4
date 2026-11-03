require 'tmpdir'
require_relative 'support/desktop'

RSpec.describe 'Attaching files from the desktop picker', :desktop do
  PIXEL_PNG = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=='.unpack1('m0')

  around do |example|
    Dir.mktmpdir('sapiens-attach') { |dir| @files = File.realpath(dir); example.run }
  end

  before do
    skip 'Native open panel contract is macOS-only' unless RUBY_PLATFORM.include?('darwin')
    @host = desktop_host
    start_desktop
  end

  def open_picker(kind)
    desktop.press('Add attachment')
    desktop.press(kind)
    desktop.find('Open', role: 'AXButton')
  end

  # The open panel is native: navigate with Go to Folder, then confirm with Open.
  def choose(path)
    desktop.control('activate')
    desktop.key('command+shift+g')
    Contract.wait(description: 'Go to Folder field focus') do
      %w[AXTextField AXComboBox].include?(desktop.control('focused')['role'])
    rescue RuntimeError => error
      raise unless error.message.include?('"code":"focus_unavailable"')
      false
    end
    desktop.control('paste', '--text', path)
    desktop.key('return')
    desktop.press('Open')
  end

  def draft(name)
    Contract.wait(description: "saved draft #{name}") do
      host.state.fetch('attachment_drafts', {}).fetch(chief, []).find { |item| item['name'] == name }
    end
  end

  {
    'an image' => {kind: 'Image', name: 'pixel.png', bytes: PIXEL_PNG, stored: 'image'},
    'a document' => {kind: 'Document', name: 'notes.txt', bytes: "Total: 42\n", stored: 'document'}
  }.each do |label, file|
    it "attaches #{label} chosen in the native open panel" do
      path = File.join(@files, file[:name])
      File.binwrite(path, file[:bytes])
      open_picker(file[:kind])
      choose(path)
      desktop.see(file[:name])
      expect(draft(file[:name])).to include('kind' => file[:stored], 'size' => file[:bytes].bytesize)
    end
  end

  it 'keeps the composer usable after the open panel is cancelled' do
    open_picker('Image')
    desktop.press('Cancel')
    desktop.press('Close dialog')
    desktop.fill('Message ChiefSapi', 'Still here')
    desktop.press('Send message')
    desktop.see('Contract test reply')
    expect(host.state.fetch('attachment_drafts', {}).fetch(chief, [])).to be_empty
  end
end
