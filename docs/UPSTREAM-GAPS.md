# Remaining gaps to upstream Harp 1.6.16

Status at the pause on 2026-10-25. The first pass matched the Home and
Journals screens; see [VERIFICATION.md](VERIFICATION.md) for what was
measured. Everything below is still different from upstream, grouped by
screen and roughly in the order it would be worth doing.

The reference setup: upstream Harp 1.6.16 from F-Droid and the debug
Companion side by side in split screen on the Pixel Tablet (light mode,
635x800 dp panes), both showing the synthetic `aisha-khan-human` profile
(upstream `assets/test-profiles`, copied to the tablet's
`Documents/harp-test` and read by harp-native from a scratch copy).
Upstream source: https://git.sr.ht/~lepisma/harp-kmp at tag `v1.6.16`.

## Entry form (JournalEntryForm in its bottom sheet)

- **Notched outlined fields.** Harp's Text, Time and Date fields are
  Material OutlinedTextFields: the label floats in a gap in the outline and
  turns red with the outline in the error state. Foundation's text field
  draws the label above the box and keeps the placeholder centred. Needs a
  notched-outline mode in Foundation's text-field decoration.
- **Time and date pickers.** Harp's fields are read-only and open a time
  picker dialog (24-hour, vertical layout) and a date picker; the date reads
  "Oct 25, 2026". harp-native's are typed `HH:MM` and `YYYY-MM-DD` fields
  with Material icons, not Font Awesome's clock and calendar.
- **Save disabled while blank.** Harp greys Save until the text has content.
- **Hash autocomplete.** Harp suggests tags and metric tokens while typing a
  `#`, and shows `#metric-id(number)` placeholders in the outline color.
- **Text field size.** Harp's field is a fixed 200dp; harp-native's is six
  lines.
- **Attachments.** The camera and link buttons do nothing yet: the
  Companion has no file or camera picker for applets. The attachment list
  under the buttons (numbered chips with a delete button) is not drawn.
- **Edit keeps heading tags.** Editing saves the entry's out-of-body heading
  tags unchanged; upstream's exact behaviour here was not checked.

## Journals tab

- **Entry menu trigger.** Harp's entry menu is Font Awesome's ellipsis at
  14dp; harp-native shows Material's `more_vert`, whose dots are larger and
  further apart. Needs a way for a `menu` to take an app glyph as its
  trigger (for example glyphs declared on the design scope).
- **Entry menu items.** Harp offers Copy text, Select, Edit, Move and
  Delete, each with a 12dp Font Awesome icon; harp-native offers Edit and
  Delete only.
- **Selection mode.** Long-pressing an entry selects it, with a bottom app
  bar for actions on the selection (including export); not implemented.
- **Journal selector.** Tapping the selector card opens Harp's journal
  management dialog (choose, create, rename, delete, reorder, set default);
  harp-native cycles to the next journal.
- **Sort.** Harp's sort button opens a dropdown (newest or oldest first,
  with a check on the current one); harp-native flips the order directly.
- **Tag filter.** Harp's filter chips select several tags at once and show
  a check icon; harp-native filters by one tag. The chips' expand animation
  is absent.
- **Search highlighting.** Harp highlights matches of the text search in
  the entry text and marks attachments whose OCR text matches.
- **Attachment chips.** Tapping one opens the file with the default
  application in Harp; harp-native's chip is not tappable.
- **Placeholder.** "No entries in this Journal" uses a 96dp faint icon whose
  size and position were not compared with Harp's PlaceholderBackground.

## Home screen

- **Open** re-reads `harp-directory` instead of Harp's Open Profile dialog
  and folder picker; **Create** opens harp-native's generic profile form
  instead of Harp's NewProfileForm dialog.
- **Forget profile** (the eraser) re-reads the directory instead of asking
  to forget the profile.
- **About and settings** both open harp-native's settings view; Harp opens
  an About dialog and its AppSettingsScreen. The intro carousel shown on
  first launch is not reproduced.
- **Shared files.** Harp's "Save shared files" dialog for files shared into
  the app has no counterpart.

## Metrics and Documents tabs

Both still render harp-native's earlier generic cards inside the new
header, filter bar and navigation bar. Upstream's MetricsView (metric cards
with plots), MetricForm, DocumentsView, DocumentCard and DocumentForm have
not been matched.

## Screens outside upstream

harp-native's Medications, Reminders and Settings views come from the
historical Harp and have no screen in upstream 1.6.16 (its Reminders tab is
commented out). They keep their earlier generic look and are reached
through the settings icon.

## Across screens

- **Dark mode.** The dark palette is the same seed's Material dark scheme,
  not measured on a device.
- **Wallpaper colors.** The light palette is the tablet's dynamic scheme as
  measured; harp-native does not follow the wallpaper.
- **Shadows in split screen.** Android lights shadows from the screen's top
  centre, so card shadows fall to the left in one pane and to the right in
  the other. This is not a styling difference.
- **Phone.** Nothing was compared on the phone; upstream Harp is not
  installed there.
- **Text metrics.** Harp and harp-native both lay text out with Compose, so
  no TextView metric table was needed; remaining 1px offsets are subpixel
  placement.
