# TJppLinkLabel

Label with additional fonts (`TFont`) for 5 states: *normal*, *visited-normal*, *hot*, *visited-hot* and *disabled*. It is inherited from `TCustomLabel`.

In the `URL` property you can enter the website address that will be displayed (in the default browser) when the user clicks the label. The `ClickActionType` property should be set to `catGoToURL`.  
In fact, after clicking the label, the text from the `URL` property is passed to the `ShellExecute` function as the third parameter (`FileName`), and the first parameter (`Operation`) is set to `open`. So, in the `URL` property you can also specify the path to a file or program to execute.

In addition, I added support for the actions: property `Action`. You can set here any action registered in the `TActionList` or `TActionManager`. The assigned action will be executed after the user clicks the label, but only if the `ClickActionType` is set to `catExecuteAction`.

Moreover, I added two cursors:
- `CursorHot` - displayed when the mouse cursor is above the enabled label (when `Enabled` = `True`).
- `CursorDisabled` - displayed when the mouse cursor is above the disabled label (when `Enabled` = `False`).

## Helper routines

In the `JPP.LinkLabel` unit, there are three auxiliary procedures: `SetJppLinkLabelFonts` and two overloaded procedures `SetJppLinkLabelColors`.

The declaration of the first procedure is as follows:
```delphi
procedure SetJppLinkLabelFonts(lbl: TJppLinkLabel; FontName: string = 'Segoe UI'; FontSize: integer = 8);
```
This procedure sets the font name and font size for all fonts available in this component: `FontNormal`, `FontVisitedNormal`,  `FontHot`, `FontVisitedHot` and `FontDisabled`.

The second procedure:
```delphi
procedure SetJppLinkLabelColors(lbl: TJppLinkLabel; clNormal, clHot, clDisabled, clVisitedNormal, clVisitedHot: TColor); overload;
procedure SetJppLinkLabelColors(lbl: TJppLinkLabel; Color: TColor); overload;
```
These procedures allow you to quickly change the color of all fonts. In the first version, you must pass color values for all label states. In the second version of this procedure, you pass only one color, which will be set for all fonts.
