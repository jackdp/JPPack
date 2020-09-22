unit JPP.HtmlHint;

{

  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp
  ----------------------------------------------------

  A lightweight HTML hint component for Delphi (in Lazarus it compiles but doesn't work).

  This unit was created by combining two other Pascal units:

  1. TFlatHintUnit.pas from the FlatStyle package
     https://github.com/jackdp/FlatStyle/blob/master/Source/TFlatHintUnit.pas
     License: Free (no additional information)

  2. HtHint.pas from the JVCL package
     https://github.com/project-jedi/jvcl/blob/master/jvcl/install/JVCLInstall/Helpers/HtHint.pas
     License: MPL 1.1

  License for my modifications: no license (public domain)

  -----------------------------------------------------

  I need HTML hint component for my open source applications. The combination of the above-mentioned files
  resulted in a component that meets my requirements. Maybe it will be useful to someone else.

  But you need to know that this is not a full-featured HTML component.
  This is a VERY simple component with support for just a few HTML tags, in addition, with a slightly unusual syntax.
  If you need more advanced free HTML components for Delphi, you can use:
  1. Lightweight solution: DzHTMLText from https://github.com/digao-dalpiaz/DzHTMLText (my fork https://github.com/jackdp/DzHTMLText2)
  2. Big but powerful: HtmlViewer from https://github.com/BerndGabriel/HtmlViewer
  3. JVCL - There are a few "HTML-ed" components (eg. TJvHTLabel, TJvHint, TJvHtButton...).
     But JVCL and JCL units are very closely related to each other and if, for example, you want to use only
     TJvHint, you will get several dozen others during compilation. So it is not especially lightweight solution.
     HTMLDrawTextEx proc. from JvJVCLUtils.pas looks very promising.

     TJvHint tip: call JvHint.RegisterHtHints in the form constructor or in the unit initialization.

  ---------------------------------------------------------

  Supported tags:

    B - <b>bold text</b>
    I - <i>italic</i>
    U - <u>underline</u>
    S - <s>strike out</s>

    C:color - text color, eg, <c:clRed>text</cl>, <cl:Red>text</c>, <c:#FF0000>text</c>, <c:255,0,0>RGB color</c>
    BG:color - background color, eg. <bg:clYellow>yellow background</bg>

    IND:x - indentation from current position. eg. <ind:10>
    AIND:x - absolute indentation, eg. <aind:20>

    FS:x - font size, eg. <fs:12>some text</fs>, <fs:+4>text</fs>
    FN:name - font name, eg. <fn:Verdana>text</fn>, <fn:Courier New>text</fn>, <fn:default>restore default font</fn>


  Metacolors (or mode-colors): Colors taken from the Appearance.DarkModeColors or Appearance.LightModeColors palette
  Example: <c:mclDarkBlue><bg:mclLightGray>dark blue text on light gray background</bg></c>

  HTML entities: see function ReplaceHtmlEntities

}



interface

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

{$I jpp.inc}


uses
  {$IFDEF FPC}
  LCLIntf, LCLType,
  {$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes, Graphics, Messages, Controls, Forms, SysUtils,
  {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  JPL.Strings, JPL.Tstr, JPL.Conversion, JPL.Colors,
  Types,
  JPP.Types, JPP.Common;


type

  // TODO : TJppHtmlHintModeColors - implement Assign
  TJppHtmlHintModeColors = class(TJppPersistent)
  private
    FOwner: TComponent;
    FBackground: TColor;
    FText: TColor;
    FBorder: TColor;
    FArrow: TColor;
    FArrowBackground: TColor;
    FArrowRectBorder: TColor;
    FmclDarkGray: TColor;
    FmclLightGray: TColor;
    FmclDarkBlue: TColor;
    FmclLightBlue: TColor;
    FmclDarkGreen: TColor;
    FmclLightGreen: TColor;
    FmclDarkRed: TColor;
    FmclLightRed: TColor;
    FmclDarkCyan: TColor;
    FmclLightCyan: TColor;
    FmclDarkMagenta: TColor;
    FmclLightMagenta: TColor;
    FmclDarkYellow: TColor;
    FmclLightYellow: TColor;
    procedure SetBackground(const Value: TColor);
    procedure SetText(const Value: TColor);
    procedure SetBorder(const Value: TColor);
    procedure SetArrow(const Value: TColor);
    procedure SetArrowBackground(const Value: TColor);
    procedure SetArrowRectBorder(const Value: TColor);
    procedure SetmclDarkGray(const Value: TColor);
    procedure SetmclLightGray(const Value: TColor);
    procedure SetmclDarkBlue(const Value: TColor);
    procedure SetmclLightBlue(const Value: TColor);
    procedure SetmclDarkGreen(const Value: TColor);
    procedure SetmclLightGreen(const Value: TColor);
    procedure SetmclDarkRed(const Value: TColor);
    procedure SetmclLightRed(const Value: TColor);
    procedure SetmclDarkCyan(const Value: TColor);
    procedure SetmclLightCyan(const Value: TColor);
    procedure SetmclDarkMagenta(const Value: TColor);
    procedure SetmclLightMagenta(const Value: TColor);
    procedure SetmclDarkYellow(const Value: TColor);
    procedure SetmclLightYellow(const Value: TColor);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Background: TColor read FBackground write SetBackground default clWhite;
    property Text: TColor read FText write SetText default clBlack;
    property Border: TColor read FBorder write SetBorder default clGray;
    property Arrow: TColor read FArrow write SetArrow default clBlack;
    property ArrowBackground: TColor read FArrowBackground write SetArrowBackground default $0053D2FF;
    property ArrowRectBorder: TColor read FArrowRectBorder write SetArrowRectBorder default $002DCBFF;
    property mclDarkGray: TColor read FmclDarkGray write SetmclDarkGray default $00666666;
    property mclLightGray: TColor read FmclLightGray write SetmclLightGray default $00BBBBBB;
    property mclDarkBlue: TColor read FmclDarkBlue write SetmclDarkBlue default $00A85400;
    property mclLightBlue: TColor read FmclLightBlue write SetmclLightBlue default $00FF8204;
    property mclDarkGreen: TColor read FmclDarkGreen write SetmclDarkGreen default $00FF8204;
    property mclLightGreen: TColor read FmclLightGreen write SetmclLightGreen default $004AC131;
    property mclDarkRed: TColor read FmclDarkRed write SetmclDarkRed default $00000093;
    property mclLightRed: TColor read FmclLightRed write SetmclLightRed default $002222FF;
    property mclDarkCyan: TColor read FmclDarkCyan write SetmclDarkCyan default $00C9BB0A;
    property mclLightCyan: TColor read FmclLightCyan write SetmclLightCyan default $00F5E732;
    property mclDarkMagenta: TColor read FmclDarkMagenta write SetmclDarkMagenta default $008B008B;
    property mclLightMagenta: TColor read FmclLightMagenta write SetmclLightMagenta default $00FF15FF;
    property mclDarkYellow: TColor read FmclDarkYellow write SetmclDarkYellow default $0000B6F2;
    property mclLightYellow: TColor read FmclLightYellow write SetmclLightYellow default $0071EDFF;
  end;


  TJppHtmlHintColorMode = (hhcmLight, hhcmDark);


  // TODO : TJppHtmlHintAppearance - implement Assign
  TJppHtmlHintAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FColorMode: TJppHtmlHintColorMode;
    FLightModeColors: TJppHtmlHintModeColors;
    FDarkModeColors: TJppHtmlHintModeColors;
    FShowArrow: Boolean;
    FMarginX: integer;
    FMarginY: integer;
    FExtraBottomMargin: integer;
    procedure SetColorMode(const Value: TJppHtmlHintColorMode);
    procedure SetLightModeColors(const Value: TJppHtmlHintModeColors);
    procedure SetDarkModeColors(const Value: TJppHtmlHintModeColors);
    procedure SetShowArrow(const Value: Boolean);
    procedure SetMarginX(const Value: integer);
    procedure SetMarginY(const Value: integer);
    procedure SetExtraBottomMargin(const Value: integer);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure RestoreDefaultLightColors;
    procedure RestoreDefaultDarkColors;
  published
    property ColorMode: TJppHtmlHintColorMode read FColorMode write SetColorMode default hhcmLight;
    property LightModeColors: TJppHtmlHintModeColors read FLightModeColors write SetLightModeColors;
    property DarkModeColors: TJppHtmlHintModeColors read FDarkModeColors write SetDarkModeColors;
    property ShowArrow: Boolean read FShowArrow write SetShowArrow default True;
    property MarginX: integer read FMarginX write SetMarginX default 4;
    property MarginY: integer read FMarginY write SetMarginY default 4;
    property ExtraBottomMargin: integer read FExtraBottomMargin write SetExtraBottomMargin default 0;
  end;


  TJppHtmlHint = class(TComponent)
  private
    FOriginalHintWindowClass: THintWindowClass;
    FFont: TFont;
    FOnShowHint: TShowHintEvent;
    FAppearance: TJppHtmlHintAppearance;
    FEnabled: Boolean;
    procedure SetFont (Value: TFont);
    procedure GetHintInfo (var HintStr: string; var CanShow: Boolean; var HintInfo: Controls.THintInfo);
    procedure SetAppearance(const Value: TJppHtmlHintAppearance);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function ModeColors: TJppHtmlHintModeColors;
    function GetModeColorByName(const mclColorName: string; ColorInvalid: TColor = clNone): TColor;
  published
    property Font: TFont read FFont write SetFont;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property Appearance: TJppHtmlHintAppearance read FAppearance write SetAppearance;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;


  TJppHtmlHintWindow = class(THintWindow)
  private
    type
      TArrowPos = (NE, NW, SE, SW);
  private
    FArrowPos: TArrowPos;
    FArrowPoint: TPoint;
    FHint: TJppHtmlHint;
    FPlainTextWidth: integer;
    FLineCount: integer;
  protected
    function FindHintComponent: TJppHtmlHint;
    procedure Paint; override;
    procedure CreateParams (var Params: TCreateParams); override;
    procedure UpdatePlainTextWidth(AHint: string);
  public
    procedure ActivateHint (HintRect: TRect; const AHint: string); override;

    procedure ItemHtDraw(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean);
    procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean; var PlainItem: string;
      var Width: Integer; CalcWidth: Boolean);
  end;



implementation

var
  HintControl: TControl; // control the tooltip belongs to


// Returns a substring. Substrings are divided by Sep character [translated]
function SubStr(const S: string; const Index: Integer; const Separator: string): string;
var
  I: Integer;
  pB, pE: PChar;
begin
  Result := '';
  if ((Index < 0) or ((Index = 0) and (Length(S) > 0) and (S[1] = Separator))) or (Length(S) = 0) then Exit;
  pB := PChar(S);
  for I := 1 to Index do
  begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then Exit;
    pB := pB + Length(Separator);
    if pB[0] = #0 then Exit;
  end;
  pE := StrPos(pB + 1, PChar(Separator));
  if pE = nil then pE := PChar(S) + Length(S);
  if not(AnsiStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then SetString(Result, pB, pE - pB);
end;


{$Region '                           TJppHtmlHint                            '}
constructor TJppHtmlHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAppearance := TJppHtmlHintAppearance.Create(AOwner);

  if not (csDesigning in ComponentState) then
  begin
    FOriginalHintWindowClass := HintWindowClass;
    HintWindowClass := TJppHtmlHintWindow;

    with Application do
    begin
      ShowHint := not ShowHint;
      ShowHint := not ShowHint;
      OnShowHint := GetHintInfo;

      HintShortPause := 25;
      HintPause := 500;
      HintHidePause := 5000;
    end;
  end;

  //FHintWidth := 200;

  FFont := TFont.Create;
  FFont.Color := ModeColors.Text;

  FEnabled := True;
end;

destructor TJppHtmlHint.Destroy;
begin
  FAppearance.Free;
  FFont.Free;
  inherited Destroy;
end;




procedure TJppHtmlHint.SetAppearance(const Value: TJppHtmlHintAppearance);
begin
  FAppearance := Value;
end;


procedure TJppHtmlHint.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if not (csDesigning in ComponentState) then
  begin
    if Value then HintWindowClass := TJppHtmlHintWindow
    else HintWindowClass := FOriginalHintWindowClass;
  end;
end;


procedure TJppHtmlHint.SetFont (Value: TFont);
begin
  FFont.Assign(Value);
  FFont.Color := ModeColors.Text;
end;


procedure TJppHtmlHint.GetHintInfo (var HintStr: string; var CanShow: Boolean; var HintInfo: Controls.THintInfo);
begin
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
  HintControl := HintInfo.HintControl;
end;

function TJppHtmlHint.GetModeColorByName(const mclColorName: string; ColorInvalid: TColor): TColor;
var
  s: string;
  mc: TJppHtmlHintModeColors;
begin
  s := TStr.TrimAndLow(mclColorName);
  if Copy(s, 1, 3) <> 'mcl' then
  begin
    Result := ColorInvalid;  // must be the "MCL" prefix
    Exit;
  end;

  mc := ModeColors;

  if s = 'mcldarkgray' then Result := mc.mclDarkGray               // mclDarkGray
  else if s = 'mcldarkgrey' then Result := mc.mclDarkGray          // mclDarkGray
  else if s = 'mcllightgray' then Result := mc.mclLightGray        // mclLightGray
  else if s = 'mcllightgrey' then Result := mc.mclLightGray        // mclLightGrey
  else if s = 'mclgrey' then Result := mc.mclLightGray             // mclGrey
  else if s = 'mclgray' then Result := mc.mclLightGray             // mclGray
  else if s = 'mclsilver' then Result := mc.mclLightGray           // mclSilver

  else if s = 'mcldarkblue' then Result := mc.mclDarkBlue          // mclDarkBlue
  else if s = 'mcllightblue' then Result := mc.mclLightBlue        // mclLightBlue
  else if s = 'mclblue' then Result := mc.mclLightBlue             // mclBlue

  else if s = 'mcldarkgreen' then Result := mc.mclDarkGreen        // mclDarkGreen
  else if s = 'mcllightgreen' then Result := mc.mclLightGreen      // mclLightGreen
  else if s = 'mclgreen' then Result := mc.mclLightGreen           // mclGreen

  else if s = 'mcldarkred' then Result := mc.mclDarkRed            // mclDarkRed
  else if s = 'mcllightred' then Result := mc.mclLightRed          // mclLightRed
  else if s = 'mclred' then Result := mc.mclLightRed               // mclRed

  else if s = 'mcldarkcyan' then Result := mc.mclDarkCyan          // mclDarkCyan
  else if s = 'mcllightcyan' then Result := mc.mclLightCyan        // mclLightCyan
  else if s = 'mclcyan' then Result := mc.mclLightCyan             // mclCyan
  else if s = 'mclaqua' then Result := mc.mclLightCyan             // mclAqua

  else if s = 'mcldarkmagenta' then Result := mc.mclDarkMagenta    // mclDarkMagenta
  else if s = 'mcllightmagenta' then Result := mc.mclLightMagenta  // mclLightMagenta
  else if s = 'mclmagenta' then Result := mc.mclLightMagenta       // mclMagenta
  else if s = 'mclfuchsia' then Result := mc.mclLightMagenta       // mclFuchia

  else if s = 'mcldarkyellow' then Result := mc.mclDarkYellow      // mclDarkYellow
  else if s = 'mcllightyellow' then Result := mc.mclLightYellow    // mclLightYellow
  else if s = 'mclyellow' then Result := mc.mclLightYellow         // mclYellow


  else Result := ColorInvalid;
end;

function TJppHtmlHint.ModeColors: TJppHtmlHintModeColors;
begin
  if FAppearance.ColorMode = hhcmLight then Result := FAppearance.LightModeColors
  else Result := FAppearance.DarkModeColors;
end;

{$endregion TJppHint}



function ReplaceHtmlEntities(A: String): String;
var
  rf: TReplaceFlags;
begin
  rf := [rfReplaceAll {, rfIgnoreCase}];  // HTML entities are case sensitive

  //Allow tag characters at text
  A := StringReplace(A, '&lt;', '<', rf);
  A := StringReplace(A, '&gt;', '>', rf);

  A := StringReplace(A, '&euro;', '€', rf);
  A := StringReplace(A, '&cent;', '¢', rf);
  A := StringReplace(A, '&pound;', '£', rf);
  A := StringReplace(A, '&yen;', '¥', rf);
  A := StringReplace(A, '&amp;', '&', rf);
  A := StringReplace(A, '&copy;', '©', rf);
  A := StringReplace(A, '&reg;', '®', rf);
  A := StringReplace(A, '&sect;', '§', rf);
  A := StringReplace(A, '&deg;', '°', rf);
  A := StringReplace(A, '&sup2;', '²', rf);
  A := StringReplace(A, '&sup3;', '³', rf);
  A := StringReplace(A, '&Integral;', '∫', rf);

  A := StringReplace(A, '&micro;', 'µ', rf);
  A := StringReplace(A, '&para;', '¶', rf);
  A := StringReplace(A, '&middot;', '·', rf);
  A := StringReplace(A, '&plusmn;', '±', rf);
  A := StringReplace(A, '&times;', '×', rf);
  A := StringReplace(A, '&divide;', '÷', rf);
  A := StringReplace(A, '&Omega;', 'Ω', rf);
  A := StringReplace(A, '&alpha;', 'α', rf);
  A := StringReplace(A, '&beta;', 'β', rf);
  A := StringReplace(A, '&gamma;', 'γ', rf);
  A := StringReplace(A, '&Gamma;', 'Γ', rf);
  A := StringReplace(A, '&delta;', 'δ', rf);
  A := StringReplace(A, '&Delta;', 'Δ', rf);
  A := StringReplace(A, '&pi;', 'π', rf);
  A := StringReplace(A, '&Pi;', 'Π', rf);
  A := StringReplace(A, '&Sigma;', 'Σ', rf);
  A := StringReplace(A, '&bull;', '•', rf);
  A := StringReplace(A, '&ndash;', '–', rf);
  A := StringReplace(A, '&trade;', '™', rf);
  A := StringReplace(A, '&SmallCircle;', '∘', rf); // &#8728; / &#x02218;

  Result := A;
end;


{$Region '                            TJppHtmlHintWindow                                 '}

function TJppHtmlHintWindow.FindHintComponent: TJppHtmlHint;
var
  FormCntr, currentComponent: Integer;
begin
  Result := nil;
  for FormCntr:= Pred(Screen.FormCount) downto 0 do
  with Screen.Forms[FormCntr] do
  begin
    for currentComponent := 0 to ComponentCount - 1 do
    if Components[currentComponent] is TJppHtmlHint then
    begin
      Result := TJppHtmlHint(Components[currentComponent]);
      Canvas.Font.Assign(Result.Font);
      Break;
    end;
  end;
end;

procedure TJppHtmlHintWindow.ItemHtDraw(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean);
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

  {$Region ' ----------------------------------- ItemHtDrawEx ----------------------------------------- '}
procedure TJppHtmlHintWindow.ItemHtDrawEx(Canvas: TCanvas; Rect: TRect; const State: TOwnerDrawState; const Text: string; const HideSelColor: Boolean;
  var PlainItem: string; var Width: Integer; CalcWidth: Boolean);
var
  TagValue, uTagValue: string;
  I: Integer;
  M1: string;
  OriRect: TRect;
  LastFontStyle: TFontStyles;
  LastFontColor: TColor;
  AColor: TColor;
  x: integer;
  bPlus, bMinus: Boolean;

  function Cmp(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
    if Result then Inc(I, Length(M1));
  end;

  function CmpL(M1: string): Boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1: string): Boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  //procedure Draw(const M: string);
  procedure Draw(M: string);
  begin
    if not Assigned(Canvas) then Exit;
    if Pos('&', M) > 0 then M := ReplaceHtmlEntities(M);
    if not CalcWidth then Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left := Rect.Left + Canvas.TextWidth(M)                     +1;   // -------------------------------------------------------
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if not Assigned(Canvas) then Exit;
    if Include then Canvas.Font.Style := Canvas.Font.Style + [Style]
    else Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

  function GetColor(s: string; Default: TColor = clBlack): TColor;
  begin
    if TryGetColor(s, Result) then Exit;
    if UpperCase(Copy(s, 1, 2)) <> 'CL' then s := 'cl' + s;
    try
      Result := StringToColor(s);
    except
      Result := Default;
    end;
  end;

begin
  if not Assigned(Canvas) then Exit;



  PlainItem := '';
  LastFontColor := 0; { satisfy compiler }


  if Canvas <> nil then
  begin
    LastFontStyle := Canvas.Font.Style;
    LastFontColor := Canvas.Font.Color;
  end;

  try

    if HideSelColor and Assigned(Canvas) then
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Font.Color := clWindowText;
    end;

    Canvas.Brush.Color := FHint.ModeColors.Background;
    //Canvas.Rectangle(Rect);

    //if Assigned(Canvas) then Canvas.FillRect(Rect);

//  OriRect := Rect; //save origin rectangle
//  Rect.Left := Rect.Left + 4;
//  Rect.Top := Rect.Top + 2;

  //InflateRect(Rect, -4, -2);



       InflateRect(Rect, 0, -1);
       Rect.Left := Rect.Left + 3;

    Width := Rect.Left;
//    Rect.Left := Rect.Left + 2;
    OriRect := Rect; //save origin rectangle

    M1 := '';
    I := 1;

    while I <= Length(Text) do
    begin

      if
        (Text[I] = '<') and
        (
          CmpL('b') or CmpL('/b') or          // bold                <b>text</b>
          CmpL('i') or CmpL('/i') or          // italic              <i>text</i>
          CmpL('u') or CmpL('/u') or          // underline           <u>text</u>
          CmpL('s') or CmpL('/s') or          // strikeout           <s>text</s>
          Cmp('c:') or CmpL('/c') or          // text color          <c:#FF0000>text</c>      <c:red>text</c>
          Cmp('bg:') or CmpL('/bg') or        // background color    <bg:#00aa33>text</bg>
          Cmp('fn:') or CmpL('/fn') or        // font name           <fn:Courier New>text</fn>
          Cmp('fs:') or CmpL('/fs') or        // font size           <fs:+3>text</fs>
          Cmp('ind:') or                      // indentation         from the current position
          Cmp('aind')                         // absolute indentation
        )
      then

      begin

        Draw(M1);
        PlainItem := PlainItem + M1;


        if CmpL1('b') then Style(fsBold, True)
        else if CmpL1('/b') then Style(fsBold, False)
        else if CmpL1('i') then Style(fsItalic, True)
        else if CmpL1('/i') then Style(fsItalic, False)
        else if CmpL1('u') then Style(fsUnderline, True)
        else if CmpL1('/u') then Style(fsUnderline, False)
        else if CmpL1('s') then Style(fsStrikeOut, True)
        else if CmpL1('/s') then Style(fsStrikeOut, False)



        // --------------------------- Font Color --------------------------

        // restore default font color
        else if CmpL1('/c') then
        begin
          Canvas.Font.Color := FHint.ModeColors.Text;
        end

        // apply new font color
        else if Cmp1('c:') then
        begin
          TagValue := SubStr(PChar(Text) + I, 0, '>');
          if (HideSelColor or not(odSelected in State)) and Assigned(Canvas) then
          begin
            uTagValue := Trim(UpperCase(TagValue));
            if uTagValue = 'DEFAULT' then AColor := FHint.ModeColors.Text
            else if Copy(uTagValue, 1, 3) = 'MCL' then AColor := FHint.GetModeColorByName(uTagValue, clNone)
            else AColor := GetColor(TagValue, clNone);
            if AColor <> clNone then Canvas.Font.Color := AColor;
          end;
          Inc(I, Length(TagValue) + 1); // >
        end


        // ------------------------- background color -----------------------

        // restore default background color
        else if CmpL1('/bg') then
        begin
          Canvas.Brush.Color := FHint.ModeColors.Background;
        end

        // apply new background color
        else if Cmp1('bg:') then
        begin
          TagValue := SubStr(PChar(Text) + I, 0, '>');
          if (HideSelColor or not(odSelected in State)) and Assigned(Canvas) then
          begin
            uTagValue := Trim(UpperCase(TagValue));
            if uTagValue = 'DEFAULT' then AColor := FHint.ModeColors.Background
            else if Copy(uTagValue, 1, 3) = 'MCL' then AColor := FHint.GetModeColorByName(uTagValue, clNone)
            else AColor := GetColor(TagValue, clNone);
            if AColor <> clNone then Canvas.Brush.Color := AColor;
          end;
          Inc(I, Length(TagValue) + 1); // >
        end



        // ---------------- font name ---------------------

        else if CmpL1('/fn') then
        begin
          Canvas.Font.Name := FHint.Font.Name;
        end

        else if Cmp1('fn:') then
        begin
          TagValue := SubStr(PChar(Text) + I, 0, '>');
          TagValue := UnquoteStr(TagValue);
          if (HideSelColor or not(odSelected in State)) and Assigned(Canvas) then
          begin
            if UpperCase(TagValue) = 'DEFAULT' then Canvas.Font.Name := FHint.Font.Name
            else Canvas.Font.Name := TagValue;
          end;
          Inc(I, Length(TagValue) + 1); // >
        end



        // ------------------- font size ------------------------

        else if CmpL1('/fs') then
        begin
          Canvas.Font.Size := FHint.Font.Size;
        end

        else if Cmp1('fs:') then
        begin
          TagValue := SubStr(PChar(Text) + I, 0, '>');
          TagValue := Trim(TagValue);

          if (Length(TagValue) > 0) and (HideSelColor or not(odSelected in State)) and Assigned(Canvas) then
          begin
            bPlus := False;
            bMinus := False;

            if Copy(TagValue, 1, 1) = '+' then
            begin
              bPlus := True;
              TagValue := Copy(TagValue, 2, Length(TagValue));
              Inc(I, 1);
            end
            else if Copy(TagValue, 1, 1) = '-' then
            begin
              bMinus := True;
              TagValue := Copy(TagValue, 2, Length(TagValue));
              Inc(I, 1);
            end;

            if TryStrToInt(TagValue, x) then
            begin
              LimitValue(x, 2, 100);
              if bPlus then Canvas.Font.Size := Canvas.Font.Size + x
              else if bMinus then Canvas.Font.Size := Canvas.Font.Size - x
              else Canvas.Font.Size := x;
            end;
          end;
          Inc(I, Length(TagValue) + 1); // >
        end


        // ------------------ indent ----------------
        // Indentation from the current position

        else if Cmp1('ind:') then
        begin
          TagValue := SubStr(PChar(Text) + I, 0, '>');
          if (HideSelColor or not(odSelected in State)) and Assigned(Canvas) then
            if TryStrToInt(TagValue, x) then Rect.Left := Rect.Left + x;

          Inc(I, Length(TagValue) + 1); // >
        end


        // ------------------ absolute indent ----------------
        // Indentation from the beginning of the line

        else if Cmp1('aind:') then
        begin
          TagValue := SubStr(PChar(Text) + I, 0, '>');
          if (HideSelColor or not(odSelected in State)) and Assigned(Canvas) then
            if TryStrToInt(TagValue, x) then Rect.Left := x;

          Inc(I, Length(TagValue) + 1); // >
        end;



        Inc(I);


        if (Text[I] = Chr(13)) and Cmp1(string(Chr(10))) then
        begin
          Rect.Left := OriRect.Left;
          Rect.Top := Rect.Top + Canvas.TextHeight(M1 + 'W');
          Inc(I);
        end;

        Dec(I);
        M1 := '';

      end

      else

        // next lines were added
        if (Text[I] = #13) or (Text[I] = #10) then
        begin
          if Text[I] = #13 then Cmp1(string(#10));
          // new line
          Draw(M1);
          PlainItem := PlainItem + M1;
          Rect.Left := OriRect.Left;
          Rect.Top := Rect.Top + Canvas.TextHeight(M1 + 'W');
          M1 := '';
        end
        else M1 := M1 + Text[I]; // add text

      Inc(I);

    end; // while


    Draw(M1);
    PlainItem := PlainItem + M1;


  finally

    if Canvas <> nil then
    begin
      if not CalcWidth then Canvas.FillRect(Rect);
      Canvas.Font.Style := LastFontStyle;
      Canvas.Font.Color := LastFontColor;
    end;

  end;


  Width := Rect.Left - Width + 2;
end;
  {$endregion ItemHtDrawEx}


procedure TJppHtmlHintWindow.CreateParams (var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style - WS_BORDER;
end;

procedure TJppHtmlHintWindow.Paint;
var
  ArrowRect, TextRect: TRect;
begin
  // Set the Rect's
  if FHint.Appearance.ShowArrow then
  begin

    case FArrowPos of
      NW, SW:
        begin
          ArrowRect := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Left + 15, ClientRect.Bottom - 1);
          TextRect  := Rect(ClientRect.Left + 15, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
        end;
      NE, SE:
        begin
          ArrowRect := Rect(ClientRect.Right - 15, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
          TextRect  := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 15, ClientRect.Bottom - 1);
        end;
    end;

  end
  else
  begin
    TextRect := ClientRect;
    InflateRect(TextRect, -1, -1);
  end;

  TextRect.Bottom := TextRect.Bottom;// + FHint.ExtraBottomMargin-10;


  // DrawBackground
  Canvas.Brush.color := FHint.ModeColors.ArrowBackground; // FHint.FArrowBackgroundColor;
  if FHint.Appearance.ShowArrow then Canvas.FillRect(ArrowRect);
  Canvas.Brush.color := FHint.ModeColors.Background; // FHint.FBackgroundColor;
  Canvas.FillRect(TextRect);


  // ---------- arrow rect border ------------
  if (FHint.Appearance.ShowArrow) and (FHint.ModeColors.ArrowRectBorder <> clNone) then //  (FHint.ColorArrowRectBorder <> clNone) then
  begin
    Canvas.Pen.Color :=  FHint.ModeColors.ArrowRectBorder; //  FHint.ColorArrowRectBorder;
    case FArrowPos of
      NE, SE:
        begin
          Canvas.MoveTo(ArrowRect.Left, ArrowRect.Top);
          Canvas.LineTo(ArrowRect.Left, ArrowRect.Bottom);
        end;
      NW, SW:
        begin
          Canvas.MoveTo(ArrowRect.Right, ArrowRect.Top);
          Canvas.LineTo(ArrowRect.Right, ArrowRect.Bottom);
        end;
    end;

  end;



  // DrawBorder
  Canvas.Brush.Color := FHint.ModeColors.Border; // FHint.FBorderColor;
  Canvas.FrameRect(ClientRect);

  // DrawArrow
  if FHint.Appearance.ShowArrow then
  begin

    case FArrowPos of
      NW: FArrowPoint := Point(ArrowRect.Left + 2, ArrowRect.Top + 2);
      NE: FArrowPoint := Point(ArrowRect.Right - 3, ArrowRect.Top + 2);
      SW: FArrowPoint := Point(ArrowRect.Left + 2, ArrowRect.Bottom - 3);
      SE: FArrowPoint := Point(ArrowRect.Right - 3, ArrowRect.Bottom - 3);
    end;

    Canvas.Pen.Color := FHint.ModeColors.Arrow; // FHint.FArrowColor;

    case FArrowPos of
      NW: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y + 6),
                           Point(FArrowPoint.x + 1, FArrowPoint.y + 6), Point(FArrowPoint.x + 1, FArrowPoint.y),
                           Point(FArrowPoint.x + 6, FArrowPoint.y),     Point(FArrowPoint.x + 6, FArrowPoint.y + 1),
                           Point(FArrowPoint.x + 2, FArrowPoint.y + 1), Point(FArrowPoint.x + 2, FArrowPoint.y + 4),
                           Point(FArrowPoint.x + 5, FArrowPoint.y + 7), Point(FArrowPoint.x + 6, FArrowPoint.y + 7),
                           Point(FArrowPoint.x + 3, FArrowPoint.y + 4), Point(FArrowPoint.x + 3, FArrowPoint.y + 3),
                           Point(FArrowPoint.x + 6, FArrowPoint.y + 6), Point(FArrowPoint.x + 7, FArrowPoint.y + 6),
                           Point(FArrowPoint.x + 3, FArrowPoint.y + 2), Point(FArrowPoint.x + 4, FArrowPoint.y + 2),
                           Point(FArrowPoint.x + 7, FArrowPoint.y + 5), Point(FArrowPoint.x + 7, FArrowPoint.y + 6)]);
      NE: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y + 6),
                           Point(FArrowPoint.x - 1, FArrowPoint.y + 6), Point(FArrowPoint.x - 1, FArrowPoint.y),
                           Point(FArrowPoint.x - 6, FArrowPoint.y),     Point(FArrowPoint.x - 6, FArrowPoint.y + 1),
                           Point(FArrowPoint.x - 2, FArrowPoint.y + 1), Point(FArrowPoint.x - 2, FArrowPoint.y + 4),
                           Point(FArrowPoint.x - 5, FArrowPoint.y + 7), Point(FArrowPoint.x - 6, FArrowPoint.y + 7),
                           Point(FArrowPoint.x - 3, FArrowPoint.y + 4), Point(FArrowPoint.x - 3, FArrowPoint.y + 3),
                           Point(FArrowPoint.x - 6, FArrowPoint.y + 6), Point(FArrowPoint.x - 7, FArrowPoint.y + 6),
                           Point(FArrowPoint.x - 3, FArrowPoint.y + 2), Point(FArrowPoint.x - 4, FArrowPoint.y + 2),
                           Point(FArrowPoint.x - 7, FArrowPoint.y + 5), Point(FArrowPoint.x - 7, FArrowPoint.y + 6)]);
      SW: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y - 6),
                           Point(FArrowPoint.x + 1, FArrowPoint.y - 6), Point(FArrowPoint.x + 1, FArrowPoint.y),
                           Point(FArrowPoint.x + 6, FArrowPoint.y),     Point(FArrowPoint.x + 6, FArrowPoint.y - 1),
                           Point(FArrowPoint.x + 2, FArrowPoint.y - 1), Point(FArrowPoint.x + 2, FArrowPoint.y - 4),
                           Point(FArrowPoint.x + 5, FArrowPoint.y - 7), Point(FArrowPoint.x + 6, FArrowPoint.y - 7),
                           Point(FArrowPoint.x + 3, FArrowPoint.y - 4), Point(FArrowPoint.x + 3, FArrowPoint.y - 3),
                           Point(FArrowPoint.x + 6, FArrowPoint.y - 6), Point(FArrowPoint.x + 7, FArrowPoint.y - 6),
                           Point(FArrowPoint.x + 3, FArrowPoint.y - 2), Point(FArrowPoint.x + 4, FArrowPoint.y - 2),
                           Point(FArrowPoint.x + 7, FArrowPoint.y - 5), Point(FArrowPoint.x + 7, FArrowPoint.y - 6)]);
      SE: canvas.Polyline([Point(FArrowPoint.x,     FArrowPoint.y),     Point(FArrowPoint.x, FArrowPoint.y - 6),
                           Point(FArrowPoint.x - 1, FArrowPoint.y - 6), Point(FArrowPoint.x - 1, FArrowPoint.y),
                           Point(FArrowPoint.x - 6, FArrowPoint.y),     Point(FArrowPoint.x - 6, FArrowPoint.y - 1),
                           Point(FArrowPoint.x - 2, FArrowPoint.y - 1), Point(FArrowPoint.x - 2, FArrowPoint.y - 4),
                           Point(FArrowPoint.x - 5, FArrowPoint.y - 7), Point(FArrowPoint.x - 6, FArrowPoint.y - 7),
                           Point(FArrowPoint.x - 3, FArrowPoint.y - 4), Point(FArrowPoint.x - 3, FArrowPoint.y - 3),
                           Point(FArrowPoint.x - 6, FArrowPoint.y - 6), Point(FArrowPoint.x - 7, FArrowPoint.y - 6),
                           Point(FArrowPoint.x - 3, FArrowPoint.y - 2), Point(FArrowPoint.x - 4, FArrowPoint.y - 2),
                           Point(FArrowPoint.x - 7, FArrowPoint.y - 5), Point(FArrowPoint.x - 7, FArrowPoint.y - 6)]);
    end;

  end;


    TextRect.Left := TextRect.Left + FHint.Appearance.MarginX div 2;
    TextRect.Top := TextRect.Top + FHint.Appearance.MarginY div 2;

  ItemHtDraw(Canvas, TextRect, [odDefault], Text, False);


  // DrawHintText
  //canvas.brush.Style := bsClear;
  //InflateRect(TextRect, -3, -1);
  //
  //if BidiMode = bdRightToLeft then
  //  DrawText(canvas.handle, PChar(Caption), Length(Caption), TextRect, DT_RIGHT or DT_WORDBREAK or DT_NOPREFIX)
  //else
  //  DrawText(canvas.handle, PChar(Caption), Length(Caption), TextRect, DT_WORDBREAK or DT_NOPREFIX);
end;



procedure TJppHtmlHintWindow.UpdatePlainTextWidth(AHint: string);
var
  S: string;
  R: TRect;
  W, xMax: Integer;
  I: Integer;
  Lines: TStrings;
begin

  FHint := FindHintComponent;

  //R := Rect(2, 2, MaxWidth - 2, MaxInt);
  R := Rect(2, 2, 00, 000);

  xMax := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := AHint;
    //W := Result.Right;
    //Result.Right := 0;
    W := xMax;
    FLineCount := Lines.Count;

    for I := 0 to Lines.Count - 1 do
    begin
      ItemHtDrawEx(Canvas, R, [odDefault], Lines[I], False, S, W, True);
      //if W > Result.Right then Result.Right := W;
      //W := Canvas.TextWidth(Lines[I]);
      if W > xMax then xMax := W;
    end;

  finally
    Lines.Free;
  end;

  //Inc(Result.Right, 6);
  FPlainTextWidth := xMax;
end;


procedure TJppHtmlHintWindow.ActivateHint(HintRect: TRect; const AHint: string);
var
  curWidth: Byte;
  Pnt: TPoint;
  HintHeight, HintWidth: Integer;
  NordWest, NordEast, SouthWest, SouthEast: TRect;
  //DeltaSizeX, DeltaSizeY, i: integer;
begin
  Caption := AHint;
  FHint := FindHintComponent;

  if FHint <> nil then
  begin

    Canvas.Font.Assign(FHint.Font);
    Canvas.Font.Color := FHint.ModeColors.Text;

    // Calculate width and height
    //HintRect.Right := HintRect.Left + 2000; //FHint.FHintWidth - 22;

    UpdatePlainTextWidth(AHint);
    HintRect.Right := HintRect.Left + FPlainTextWidth;

//    if BidiMode = bdRightToLeft then DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_RIGHT or DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX)
//    else DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
//
//    DrawText(Canvas.Handle, @AHint[1], Length(AHint), HintRect, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);

    if FHint.Appearance.ShowArrow then Inc(HintRect.Right, 22 + FHint.Appearance.MarginX) else Inc(HintRect.Right, 7 + FHint.Appearance.MarginX);
    //Inc(HintRect.Bottom, 3 + DeltaSizeY);

    // Divide the screen in 4 pices
    NordWest :=  Rect(0, 0, Screen.Width div 2, Screen.Height div 2);
    NordEast :=  Rect(Screen.Width div 2, 0, Screen.Width, Screen.Height div 2);
    SouthWest := Rect(0, Screen.Height div 2, Screen.Width div 2, Screen.Height);
    SouthEast := Rect(Screen.Width div 2, Screen.Height div 2, Screen.Width, Screen.Height);

    GetCursorPos(Pnt);

    if PtInRect(NordWest, Pnt) then FArrowPos := NW
    else if PtInRect(NordEast, Pnt) then FArrowPos := NE
    else if PtInRect(SouthWest, Pnt) then FArrowPos := SW
    else FArrowPos := SE;

    // Calculate the position of the hint
    if FArrowPos = NW then curWidth := 12
    else curWidth := 5;

    //HintHeight := HintRect.Bottom - HintRect.Top;
    HintHeight := FLineCount * Canvas.TextHeight('jl') + FHint.Appearance.MarginY + 4 + FHint.Appearance.ExtraBottomMargin;
    HintWidth  := HintRect.Right - HintRect.Left;

    case FArrowPos of
      NW: HintRect := Rect(Pnt.x + curWidth, Pnt.y + curWidth, Pnt.x + HintWidth + curWidth, Pnt.y + HintHeight + curWidth);
      NE: HintRect := Rect(Pnt.x - HintWidth - curWidth, Pnt.y + curWidth, Pnt.x - curWidth, Pnt.y + HintHeight + curWidth);
      SW: HintRect := Rect(Pnt.x + curWidth, Pnt.y - HintHeight - curWidth, Pnt.x + HintWidth + curWidth, Pnt.y - curWidth);
      SE: HintRect := Rect(Pnt.x - HintWidth - curWidth, Pnt.y - HintHeight - curWidth, Pnt.x - curWidth, Pnt.y - curWidth);
    end;

    BoundsRect := HintRect;

    Pnt := ClientToScreen(Point(0, 0));

    SetWindowPos(Handle, HWND_TOPMOST, Pnt.X, Pnt.Y, 0, 0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);

  end;

end;

{$endregion TJppHtmlHintWindow}



{$Region '                             TJppHtmlHintModeColors                               '}

constructor TJppHtmlHintModeColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TJppHtmlHintModeColors.Destroy;
begin
  inherited;
end;

procedure TJppHtmlHintModeColors.SetArrow(const Value: TColor);
begin
  FArrow := Value;
end;

procedure TJppHtmlHintModeColors.SetArrowBackground(const Value: TColor);
begin
  FArrowBackground := Value;
end;

procedure TJppHtmlHintModeColors.SetArrowRectBorder(const Value: TColor);
begin
  FArrowRectBorder := Value;
end;

procedure TJppHtmlHintModeColors.SetBackground(const Value: TColor);
begin
  FBackground := Value;
end;

procedure TJppHtmlHintModeColors.SetBorder(const Value: TColor);
begin
  FBorder := Value;
end;

procedure TJppHtmlHintModeColors.SetmclDarkBlue(const Value: TColor);
begin
  FmclDarkBlue := Value;
end;

procedure TJppHtmlHintModeColors.SetmclDarkCyan(const Value: TColor);
begin
  FmclDarkCyan := Value;
end;

procedure TJppHtmlHintModeColors.SetmclDarkGray(const Value: TColor);
begin
  FmclDarkGray := Value;
end;

procedure TJppHtmlHintModeColors.SetmclDarkGreen(const Value: TColor);
begin
  FmclDarkGreen := Value;
end;

procedure TJppHtmlHintModeColors.SetmclDarkMagenta(const Value: TColor);
begin
  FmclDarkMagenta := Value;
end;

procedure TJppHtmlHintModeColors.SetmclDarkRed(const Value: TColor);
begin
  FmclDarkRed := Value;
end;

procedure TJppHtmlHintModeColors.SetmclDarkYellow(const Value: TColor);
begin
  FmclDarkYellow := Value;
end;

procedure TJppHtmlHintModeColors.SetmclLightBlue(const Value: TColor);
begin
  FmclLightBlue := Value;
end;

procedure TJppHtmlHintModeColors.SetmclLightCyan(const Value: TColor);
begin
  FmclLightCyan := Value;
end;

procedure TJppHtmlHintModeColors.SetmclLightGray(const Value: TColor);
begin
  FmclLightGray := Value;
end;

procedure TJppHtmlHintModeColors.SetmclLightGreen(const Value: TColor);
begin
  FmclLightGreen := Value;
end;

procedure TJppHtmlHintModeColors.SetmclLightMagenta(const Value: TColor);
begin
  FmclLightMagenta := Value;
end;

procedure TJppHtmlHintModeColors.SetmclLightRed(const Value: TColor);
begin
  FmclLightRed := Value;
end;

procedure TJppHtmlHintModeColors.SetmclLightYellow(const Value: TColor);
begin
  FmclLightYellow := Value;
end;

procedure TJppHtmlHintModeColors.SetText(const Value: TColor);
begin
  FText := Value;
end;

{$endregion TJppHtmlHintModeColors}



{$region '                            TJppHtmlHintAppearance                          '}

constructor TJppHtmlHintAppearance.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FColorMode := hhcmLight;

  FLightModeColors := TJppHtmlHintModeColors.Create(AOwner);
  FDarkModeColors := TJppHtmlHintModeColors.Create(AOwner);

  RestoreDefaultLightColors;
  RestoreDefaultDarkColors;

  FShowArrow := True;
  FMarginX := 4;
  FMarginY := 4;
  FExtraBottomMargin := 0;
end;

destructor TJppHtmlHintAppearance.Destroy;
begin
  FLightModeColors.Free;
  FDarkModeColors.Free;
  inherited;
end;

procedure TJppHtmlHintAppearance.RestoreDefaultDarkColors;
begin
  FDarkModeColors.Background := $003B3B3B;
  FDarkModeColors.Text := $00D3D3D3;
  FDarkModeColors.Border := $00313131;
  FDarkModeColors.Arrow := $00EEEEEE;
  FDarkModeColors.ArrowBackground := $00191919;
  FDarkModeColors.ArrowRectBorder := clBlack;
  FDarkModeColors.mclDarkGray := $00707070;
  FDarkModeColors.mclLightGray := $00B9B9B9;
  FDarkModeColors.mclDarkBlue := $00F27900;
  FDarkModeColors.mclLightBlue := $00FFAE5E;
  FDarkModeColors.mclDarkGreen := $0039AC33;
  FDarkModeColors.mclLightGreen := $0057EE5B;
  FDarkModeColors.mclDarkRed := $002424FF;
  FDarkModeColors.mclLightRed := $006F6FFF;
  FDarkModeColors.mclDarkCyan := $00CECE00;
  FDarkModeColors.mclLightCyan := $00FFFF51;
  FDarkModeColors.mclDarkMagenta := $00FF1CFF;
  FDarkModeColors.mclLightMagenta := $00FF84FF;
  FDarkModeColors.mclDarkYellow := $0000BED9;
  FDarkModeColors.mclLightYellow := $006FEDFF;
end;

procedure TJppHtmlHintAppearance.RestoreDefaultLightColors;
begin
  FLightModeColors.Background := clWhite;
  FLightModeColors.Text := clBlack;
  FLightModeColors.Border := clGray;
  FLightModeColors.Arrow := clBlack;
  FLightModeColors.ArrowBackground := $0053D2FF;
  FLightModeColors.ArrowRectBorder := $002DCBFF;
  FLightModeColors.mclDarkGray := $00666666;
  FLightModeColors.mclLightGray := $00BBBBBB;
  FLightModeColors.mclDarkBlue := $00A85400;
  FLightModeColors.mclLightBlue := $00FF8204;
  FLightModeColors.mclDarkGreen := $00589D37;
  FLightModeColors.mclLightGreen := $004AC131;
  FLightModeColors.mclDarkRed := $00000093;
  FLightModeColors.mclLightRed := $002222FF;
  FLightModeColors.mclDarkCyan := $00C9BB0A;
  FLightModeColors.mclLightCyan := $00F5E732;
  FLightModeColors.mclDarkMagenta := $008B008B;
  FLightModeColors.mclLightMagenta := $00FF15FF;
  FLightModeColors.mclDarkYellow := $0000B6F2;
  FLightModeColors.mclLightYellow := $0071EDFF;
end;

procedure TJppHtmlHintAppearance.SetColorMode(const Value: TJppHtmlHintColorMode);
begin
  FColorMode := Value;
end;

procedure TJppHtmlHintAppearance.SetDarkModeColors(const Value: TJppHtmlHintModeColors);
begin
  FDarkModeColors := Value;
end;

procedure TJppHtmlHintAppearance.SetExtraBottomMargin(const Value: integer);
begin
  FExtraBottomMargin := Value;
end;

procedure TJppHtmlHintAppearance.SetLightModeColors(const Value: TJppHtmlHintModeColors);
begin
  FLightModeColors := Value;
end;

procedure TJppHtmlHintAppearance.SetMarginX(const Value: integer);
begin
  FMarginX := Value;
end;

procedure TJppHtmlHintAppearance.SetMarginY(const Value: integer);
begin
  FMarginY := Value;
end;

procedure TJppHtmlHintAppearance.SetShowArrow(const Value: Boolean);
begin
  FShowArrow := Value;
end;

{$endregion TJppHtmlHintAppearance}





end.
