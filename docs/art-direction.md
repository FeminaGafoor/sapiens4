# Sapiens4 art direction

The application follows the supplied **Sapiens4 Art Direction 2026** reference:
neutral grounds, floating round cards, keyboard faces and a single pink primary
action. `app/theme/sapi-theme.css` is the final visual layer, loaded after the
structural and feature stylesheets.

## Shared tokens

| Role | Light | Dark |
| --- | --- | --- |
| Ground | `#F2F2F2` | `#111111` |
| Card | `#FFFFFF` | `#1C1C1C` |
| Soft surface | `#F2F2F2` | `#2A2A2A` |
| Ink | `#111111` | `#F5F5F5` |
| Quiet text | `#6B6B6B` | `#A3A3A3` |
| Line | `#E6E6E6` | `#333333` |

Action/Chiefy `#F9BDD0`, blush `#F08FB0`, Curio `#CFC4F2`, Buildly `#FBE58A`
and Nerd `#BFE3FA` stay identical in both themes. Text on pastel surfaces is
always `#111111`. Blush is decorative progress color, never body text.

Cards and dialogs use 20px radii; fields, rows and inset surfaces use 14px;
buttons, tabs, chips and composers use pill shapes. Panes are separated by 12px
of ground, with no dividing borders. Visible panes alone receive grid tracks.

DM Sans is the interface font, Zen Maru Gothic is the face font (300 for artwork,
500 for chips), and IBM Plex Mono is the code font. All four WOFF2 files are
bundled with their full glyph sets. Font licenses ship in `license/fonts/`.

## Components

- Shell: header, pane controls, connection status, corpora menus, update control.
- Roster: search, scope segments, selected rows, avatar chips, Group badges,
  shared-computer card.
- Conversation: assistant text, user bubbles, activity segments and menus,
  composer, mentions, attachments, recording controls, warnings and live work.
- Work: task and routine cards, state marks, disclosures, results, code and events.
- Workspace: browser tabs, address entry, bookmarks, menus, document content,
  file previews and empty state.
- Dialogs: application menu, workspace settings, model settings, provider setup,
  Sapi/Group forms, attachments, local corpora, remote pairing and relay management.
- First run: storage choices, consent, installation, empty corpora and legal pages.
- Identity: Chiefy artwork, favicon, install icons and Windows icon resources.

Secondary actions are grey. The composer owns the pink action during chat; an
open dialog neutralizes the background send control and owns its primary action.
Stop/cancel and application-update controls stay neutral.

## Faces and accessibility

`Sapiens.Art` owns nine decorative moods plus the Chiefy SVG. Artwork has
`aria-hidden="true"`; names, status labels and instructions remain real text.
White heads and dark facial marks do not invert in dark mode. Compact avatar
chips use the stored character marks without adding outer parentheses. Existing
names and custom facial expressions remain unchanged. Legacy default palette
values are translated at render time; custom colors remain intact. No saved
application data is rewritten.

Task marks distinguish complete (`●`), active (`◎`), waiting (`○`) and attention
(`!`) in addition to their text labels. Focus uses a contrasting ink outline.
Motion is suppressed for reduced-motion preferences.

## Runtime delivery

The font files are registered in `Assets.publicAssets`, which also places them
in native packages. `/assets/fonts/` resolves the theme stylesheet's relative
font URLs. `/theme/fonts/` supports legal documents opened over HTTP and the
same relative paths in local native files. The relay and bootstrap policies
allow same-origin fonts. The remote service worker caches the font assets; its
shell version marker is kept in sync with the relay's content-hash replacement.

## Review checklist

Check light/dark themes, 320–1440px widths, all three panes and individual pane
visibility, long names, custom faces, menu placement, keyboard focus, chat and
voice controls, all dialog kinds, onboarding and offline remote typography.
Native webview geometry and platform behavior still require the desktop suite
in `docs/testing.md` when qualifying a release.

## Rendered previews

These are the actual PureScript view components rendered with sample data in
Chromium, rather than HTML mockups. They are visual fixtures, not live accounts.

[Light desktop](art-direction/light.png) · [Dark desktop](art-direction/dark.png) ·
[Mobile conversation](art-direction/mobile.png) · [Mobile model settings](art-direction/models-mobile.png)

Validation for this change: the full PureScript UI compiled and all eight
production bundles built. Browser checks covered 29 configurations (five widths
in both themes plus 19 component scenes), with no runtime errors or horizontal
overflow. Automated WCAG A/AA audits passed for the audited desktop/mobile chat
and component scenes. Additional checks covered composer target sizes and
spacing, typed input, theme/menu/dialog rendering, hidden panes, single-pane
sizing, keyboard focus, setup/legal presentation and the mobile model selectors.
Font route resolution, offline font-cache membership, relay cache-version
replacement and Windows icon checksums were verified separately. These checks
used the app's view components with fixture state; they do not replace the native
end-to-end suite, which was not run for this change.
