unit JPP.ColorControls.Common;


{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF}
  {$IFDEF DCC}
  System.SysUtils, System.Classes, System.Types,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics, Vcl.Dialogs, Vcl.Buttons, Vcl.Clipbrd, Vcl.ExtCtrls,
  {$ELSE}
  SysUtils, Classes, Types, Controls, StdCtrls, Graphics, Buttons, Clipbrd, ExtCtrls, LCLType, LCLIntf,
  {$ENDIF}

  JPL.Strings, JPL.Conversion, JPL.Colors, JPL.ColorArrays,

  JPP.Common, JPP.BasicSpeedButton, JPP.Graphics, JPP.Gradient, JPP.Types;


type

  TJppColorControlBorderMode = (bmNoBorder, bmAverageColor, bmSimpleColor);
  TJppColorControlSelectSeparator = procedure(const Index: integer; const SeparatorCaption: string) of object;
  TJppColorControlSelectChangeColor = procedure(const CurrentColor: TColor; var NewColor: TColor; var Handled: Boolean) of object;
  TJppColorControlGetColorStrValue = procedure(const Index: integer; const CurrentColor: TColor; var ColorStr: string) of object;
  //TJppColorControlBeforePaintItem = procedure(const Index: Integer; Rect: TRect; const State: TOwnerDrawState; var PaintHandled: Boolean) of object;
  //TJppColorControlAfterPaintItem = procedure(const Index: Integer; Rect: TRect; const State: TOwnerDrawState) of object;
  //TJppColorControlGetItemBackgroundColor = procedure(const Index: integer; State: TOwnerDrawState; var BgColor: TColor) of object;
  //TJppColorControlGetItemGradientColors = procedure(const Index: integer; State: TOwnerDrawState; var ColorFrom, ColorTo: TColor) of object;





  {$Region ' ------ TJppColorControlGradientItem ------ '}
  TJppColorControlGradientItem = class(TJppPersistent)
  private
    FOwner: TComponent;
    //FOnChange: TNotifyEvent;
    FBackground: TJppGradientBackground;
    //procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBackground(const Value: TJppGradientBackground);
  protected
    //procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ccgi: TJppColorControlGradientItem); reintroduce;
  published
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Background: TJppGradientBackground read FBackground write SetBackground;
  end;
  {$endregion TJppColorControlGradientItem}


  {$Region ' ------ TJppColorControlSelectedItem ------ '}
  TJppColorControlSelectedItem = class(TJppColorControlGradientItem)
  private
    //FOwner: TComponent;
    FFontColor: TColor;
    FDisabledFontColor: TColor;
    procedure SetFontColor(const Value: TColor);
    procedure SetDisabledFontColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(ccsi: TJppColorControlSelectedItem); reintroduce;
  published
    property FontColor: TColor read FFontColor write SetFontColor default clHighlightText;
    property DisabledFontColor: TColor read FDisabledFontColor write SetDisabledFontColor default clSilver;
  end;
  {$endregion TJppColorControlSelectedItem}


  {$Region ' ------ TJppColorControlCustomGradientCaptionItem ------ '}
  TJppColorControlCustomGradientCaptionItem = class(TJppColorControlGradientItem)
  private
    //FOwner: TComponent;
    FFont: TFont;
    FAlignment: TAlignment;
    FCaptionMargin: integer;
    FTextPosDeltaY: integer;
    FDisabledFontColor: TColor;
    FCaption: string;
    procedure SetFont(const Value: TFont);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaptionMargin(const Value: integer);
    procedure SetTextPosDeltaY(const Value: integer);
    procedure SetDisabledFontColor(const Value: TColor);
    procedure SetCaption(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ccgci: TJppColorControlCustomGradientCaptionItem); reintroduce;

    property Font: TFont read FFont write SetFont;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property CaptionMargin: integer read FCaptionMargin write SetCaptionMargin default 10;
    property TextPosDeltaY: integer read FTextPosDeltaY write SetTextPosDeltaY default -1;
    property DisabledFontColor: TColor read FDisabledFontColor write SetDisabledFontColor default clGray;
    property Caption: string read FCaption write SetCaption;
  end;
  {$endregion TJppColorControlCustomGradientCaptionItem}

  TJppColorControlGradientCaptionItem = class(TJppColorControlCustomGradientCaptionItem)
  published
    property OnChange;
    property Background;
    property Font;
    property Alignment;
    property CaptionMargin;
    property TextPosDeltaY;
    property DisabledFontColor;
    property Caption;
  end;

  TJppColorControlSeparatorItem = class(TJppColorControlCustomGradientCaptionItem)
  published
    property OnChange;
    property Background;
    property Font;
    property Alignment;
    property CaptionMargin;
    property TextPosDeltaY;
    property DisabledFontColor;
  end;


  {$Region ' ------ TJppColorControlRgbHexParams ------ '}
  TJppColorControlRgbHexParams = class(TJppPersistent)
  private
    FOwner: TComponent;
    //FOnChange: TNotifyEvent;
    FRgbSeparator: string;
    FPrefix: string;
    FSuffix: string;
    //procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetRgbSeparator(const Value: string);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
  protected
    //procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ccrhp: TJppColorControlRgbHexParams); reintroduce;
  published
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property RgbSeparator: string read FRgbSeparator write SetRgbSeparator;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
  end;
  {$endregion TJppColorControlRgbHexParams}


  {$Region ' ------ TJppColorControlBgrHexParams ------ '}
  TJppColorControlBgrHexParams = class(TJppPersistent)
  private
    FOwner: TComponent;
    //FOnChange: TNotifyEvent;
    FBgrSeparator: string;
    FPrefix: string;
    FSuffix: string;
    //procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBgrSeparator(const Value: string);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
  protected
    //procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ccbhp: TJppColorControlBgrHexParams); reintroduce;
  published
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property BgrSeparator: string read FBgrSeparator write SetBgrSeparator;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
  end;
  {$endregion TJppColorControlBgrHexParams}


  {$Region ' ------ TJppColorControlRgbIntParams ------ '}
  TJppColorControlRgbIntParams = class(TJppPersistent)
  private
    FOwner: TComponent;
    //FOnChange: TNotifyEvent;
    FRgbSeparator: string;
    FPaddingChar: Char;
    FPaddingLen: integer;
    FPrefix: string;
    FSuffix: string;
    //procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetRgbSeparator(const Value: string);
    procedure SetPaddingChar(const Value: Char);
    procedure SetPaddingLen(const Value: integer);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
  protected
    //procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ccrip: TJppColorControlRgbIntParams); reintroduce;
  published
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property RgbSeparator: string read FRgbSeparator write SetRgbSeparator;
    property PaddingChar: Char read FPaddingChar write SetPaddingChar default '0';
    property PaddingLen: integer read FPaddingLen write SetPaddingLen default 3;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
  end;
  {$endregion TJppColorControlRgbIntParams}


  {$Region ' ------ TJppColorControlRectangle ------ '}
  TJppColorControlRectangle = class(TJppPersistent)
  private
    FOwner: TComponent;
    FWidth: integer;
    FPaddingTop: integer;
    FPaddingBottom: integer;
    FBorderMode: TJppColorControlBorderMode;
    FBorderWidth: integer;
    FBorderColor: TColor;
    FLeftMargin: integer;
    FHideTopBorder: Boolean;
    FVisible: Boolean;
    procedure SetWidth(const Value: integer);
    procedure SetPaddingTop(const Value: integer);
    procedure SetPaddingBottom(const Value: integer);
    procedure SetBorderMode(const Value: TJppColorControlBorderMode);
    procedure SetBorderWidth(const Value: integer);
    procedure SetBorderColor(const Value: TColor);
    procedure SetLeftMargin(const Value: integer);
    procedure SetHideTopBorder(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ccr: TJppColorControlRectangle); reintroduce;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: integer read FWidth write SetWidth default 40;
    property PaddingTop: integer read FPaddingTop write SetPaddingTop default 1;
    property PaddingBottom: integer read FPaddingBottom write SetPaddingBottom default 1;
    property BorderMode: TJppColorControlBorderMode read FBorderMode write SetBorderMode default bmAverageColor;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth default 1;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property LeftMargin: integer read FLeftMargin write SetLeftMargin default 2;
    property HideTopBorder: Boolean read FHideTopBorder write SetHideTopBorder default False;
  end;
  {$endregion TJppColorControlRectangle}


  {$Region ' ------ TJppColorControlAppearance ------ '}
  TJppColorControlAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    //FOnChange: TNotifyEvent;
    FShowRgbInt: Boolean;
    FShowRgbHex: Boolean;
    FShowColorName: Boolean;
    FDataSeparator: string;
    FUseCustomNumericFont: Boolean;
    FNumericFont: TFont;
    FColorRectangle: TJppColorControlRectangle;
    FRgbIntParams: TJppColorControlRgbIntParams;
    FTextMargin: integer;
    FTextPosDeltaY: integer;
    FRgbHexParams: TJppColorControlRgbHexParams;
    FNumericTextPosDeltaY: integer;
    FSeparatorItem: TJppColorControlSeparatorItem;
    FNumericFontSelectedColor: TColor;
    FSelectedItem: TJppColorControlSelectedItem;
    FDisabledBackgroundColor: TColor;
    FDisabledFontColor: TColor;
    FShowBgrHex: Boolean;
    FBgrHexParams: TJppColorControlBgrHexParams;
    FLeftMargin: integer;
    //procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetShowRgbInt(const Value: Boolean);
    procedure SetShowRgbHex(const Value: Boolean);
    procedure SetShowColorName(const Value: Boolean);
    procedure SetDataSeparator(const Value: string);
    procedure SetUseCustomNumericFont(const Value: Boolean);
    procedure SetNumericFont(const Value: TFont);
    procedure SetColorRectangle(const Value: TJppColorControlRectangle);
    procedure SetRgbIntParams(const Value: TJppColorControlRgbIntParams);
    procedure SetTextMargin(const Value: integer);
    procedure SetTextPosDeltaY(const Value: integer);
    procedure SetRgbHexParams(const Value: TJppColorControlRgbHexParams);
    procedure SetNumericTextPosDeltaY(const Value: integer);
    procedure SetSeparatorItem(const Value: TJppColorControlSeparatorItem);
    procedure SetNumericFontSelectedColor(const Value: TColor);
    procedure SetSelectedItem(const Value: TJppColorControlSelectedItem);
    procedure SetDisabledBackgroundColor(const Value: TColor);
    procedure SetDisabledFontColor(const Value: TColor);
    procedure SetShowBgrHex(const Value: Boolean);
    procedure SetBgrHexParams(const Value: TJppColorControlBgrHexParams);
    procedure SetLeftMargin(const Value: integer);
  protected
    //procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(cca: TJppColorControlAppearance); reintroduce;
  published
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property ColorRectangle: TJppColorControlRectangle read FColorRectangle write SetColorRectangle;
    property ShowColorName: Boolean read FShowColorName write SetShowColorName default True;

    property ShowRgbInt: Boolean read FShowRgbInt write SetShowRgbInt default True;
    property ShowRgbHex: Boolean read FShowRgbHex write SetShowRgbHex default False;
    property ShowBgrHex: Boolean read FShowBgrHex write SetShowBgrHex default False;
    property RgbIntParams: TJppColorControlRgbIntParams read FRgbIntParams write SetRgbIntParams;
    property RgbHexParams: TJppColorControlRgbHexParams read FRgbHexParams write SetRgbHexParams;
    property BgrHexParams: TJppColorControlBgrHexParams read FBgrHexParams write SetBgrHexParams;

    property DataSeparator: string read FDataSeparator write SetDataSeparator;
    property UseCustomNumericFont: Boolean read FUseCustomNumericFont write SetUseCustomNumericFont default True;
    property NumericFont: TFont read FNumericFont write SetNumericFont;
    property NumericFontSelectedColor: TColor read FNumericFontSelectedColor write SetNumericFontSelectedColor default clHighlightText;
    property TextMargin: integer read FTextMargin write SetTextMargin default 6;
    property TextPosDeltaY: integer read FTextPosDeltaY write SetTextPosDeltaY default -1;
    property NumericTextPosDeltaY: integer read FNumericTextPosDeltaY write SetNumericTextPosDeltaY;
    property SeparatorItem: TJppColorControlSeparatorItem read FSeparatorItem write SetSeparatorItem;
    property SelectedItem: TJppColorControlSelectedItem read FSelectedItem write SetSelectedItem;
    property DisabledBackgroundColor: TColor read FDisabledBackgroundColor write SetDisabledBackgroundColor default $00E9E9E9;
    property DisabledFontColor: TColor read FDisabledFontColor write SetDisabledFontColor default clGray;
    property LeftMargin: integer read FLeftMargin write SetLeftMargin default 2;

  end;
  {$endregion TJppColorControlAppearance}



implementation


{$Region '                        TJppColorControlGradientItem                          '}

constructor TJppColorControlGradientItem.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FBackground := TJppGradientBackground.Create(AOwner);
  FBackground.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
end;

destructor TJppColorControlGradientItem.Destroy;
begin
  FBackground.Free;
  inherited;
end;

procedure TJppColorControlGradientItem.Assign(ccgi: TJppColorControlGradientItem);
begin
  BeginUpdate;
  try
    FBackground.Assign(ccgi.Background);
  finally
    EndUpdate;
  end;
end;

//procedure TJppColorControlGradientItem.PropsChanged(Sender: TObject);
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

procedure TJppColorControlGradientItem.SetBackground(const Value: TJppGradientBackground);
begin
  FBackground := Value;
  PropsChanged(Self);
end;
//
//procedure TJppColorControlGradientItem.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

{$endregion TJppColorControlGradientItem}



{$Region ' ------ TJppColorControlSelectedItem ------ '}

constructor TJppColorControlSelectedItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FFontColor := clHighlightText;
  FDisabledFontColor := clSilver;
end;

procedure TJppColorControlSelectedItem.Assign(ccsi: TJppColorControlSelectedItem);
begin
  BeginUpdate;
  try
    FFontColor := ccsi.FontColor;
    FDisabledFontColor := ccsi.DisabledFontColor;
    FBackground.Assign(ccsi.Background);
  finally
    EndUpdate;
  end;
end;

procedure TJppColorControlSelectedItem.SetDisabledFontColor(const Value: TColor);
begin
  if FDisabledFontColor = Value then Exit;
  FDisabledFontColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlSelectedItem.SetFontColor(const Value: TColor);
begin
  if FFontColor = Value then Exit;
  FFontColor := Value;
  PropsChanged(Self);
end;
{$endregion TJppColorControlSelectedItem}



{$Region '                    TJppColorControlCustomGradientCaptionItem                           '}

constructor TJppColorControlCustomGradientCaptionItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  //Background.OnChange := PropsChanged;
  FFont := TFont.Create;
  FFont.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FAlignment := taCenter;
  FCaptionMargin := 10;
  FTextPosDeltaY := -1;
  FDisabledFontColor := clGray;
end;

destructor TJppColorControlCustomGradientCaptionItem.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TJppColorControlCustomGradientCaptionItem.Assign(ccgci: TJppColorControlCustomGradientCaptionItem);
begin
  BeginUpdate;
  try
    FFont.Assign(ccgci.Font);
    FFont.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
    FAlignment := ccgci.Alignment;
    FCaptionMargin := ccgci.CaptionMargin;
    FCaption := ccgci.Caption;
    FTextPosDeltaY := ccgci.TextPosDeltaY;
    FDisabledFontColor := ccgci.DisabledFontColor;

    FBackground.Assign(ccgci.Background);
  finally
    EndUpdate;
  end;
end;

procedure TJppColorControlCustomGradientCaptionItem.SetAlignment(const Value: TAlignment);
begin
  if FAlignment = Value then Exit;
  FAlignment := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlCustomGradientCaptionItem.SetCaption(const Value: string);
begin
  if FCaption = Value then Exit;
  FCaption := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlCustomGradientCaptionItem.SetCaptionMargin(const Value: integer);
begin
  if FCaptionMargin = Value then Exit;
  FCaptionMargin := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlCustomGradientCaptionItem.SetDisabledFontColor(const Value: TColor);
begin
  if FDisabledFontColor = Value then Exit;
  FDisabledFontColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlCustomGradientCaptionItem.SetFont(const Value: TFont);
begin
  FFont := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlCustomGradientCaptionItem.SetTextPosDeltaY(const Value: integer);
begin
  if FTextPosDeltaY = Value then Exit;
  FTextPosDeltaY := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorControlCustomGradientCaptionItem}



{$Region '                  TJppColorControlRgbHexParams                   '}

constructor TJppColorControlRgbHexParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FRgbSeparator := '';
  FPrefix := '#';
  FSuffix := '';
end;

destructor TJppColorControlRgbHexParams.Destroy;
begin
  inherited;
end;

procedure TJppColorControlRgbHexParams.Assign(ccrhp: TJppColorControlRgbHexParams);
begin
  BeginUpdate;
  try
    FRgbSeparator := ccrhp.RgbSeparator;
    FPrefix := ccrhp.Prefix;
    FSuffix := ccrhp.Suffix;
  finally
    EndUpdate;
  end;
end;

//procedure TJppColorControlRgbHexParams.PropsChanged(Sender: TObject);
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

//procedure TJppColorControlRgbHexParams.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

procedure TJppColorControlRgbHexParams.SetPrefix(const Value: string);
begin
  FPrefix := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRgbHexParams.SetRgbSeparator(const Value: string);
begin
  FRgbSeparator := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRgbHexParams.SetSuffix(const Value: string);
begin
  FSuffix := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorControlRgbHexParams}


{$Region '                  TJppColorControlBgrHexParams                   '}

constructor TJppColorControlBgrHexParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FBgrSeparator := '';
  FPrefix := '$00';
  FSuffix := '';
end;

destructor TJppColorControlBgrHexParams.Destroy;
begin
  inherited;
end;

procedure TJppColorControlBgrHexParams.Assign(ccbhp: TJppColorControlBgrHexParams);
begin
  BeginUpdate;
  try
    FBgrSeparator := ccbhp.BgrSeparator;
    FPrefix := ccbhp.Prefix;
    FSuffix := ccbhp.Suffix;
  finally
    EndUpdate;
  end;
end;

//procedure TJppColorControlBgrHexParams.PropsChanged(Sender: TObject);
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

//procedure TJppColorControlBgrHexParams.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

procedure TJppColorControlBgrHexParams.SetPrefix(const Value: string);
begin
  FPrefix := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlBgrHexParams.SetBgrSeparator(const Value: string);
begin
  FBgrSeparator := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlBgrHexParams.SetSuffix(const Value: string);
begin
  FSuffix := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorControlBgrHexParams}



{$Region '                           TJppColorControlRgbIntParams                                '}

constructor TJppColorControlRgbIntParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FRgbSeparator := ',';
  FPaddingChar := '0';
  FPaddingLen := 3;
  FPrefix := '';
  FSuffix := '';
end;

destructor TJppColorControlRgbIntParams.Destroy;
begin
  inherited;
end;

procedure TJppColorControlRgbIntParams.Assign(ccrip: TJppColorControlRgbIntParams);
begin
  BeginUpdate;
  try
    FRgbSeparator := ccrip.RgbSeparator;
    FPaddingChar := ccrip.PaddingChar;
    FPaddingLen := ccrip.PaddingLen;
    FPrefix := ccrip.Prefix;
    FSuffix := ccrip.Suffix;
  finally
    EndUpdate;
  end;
end;

//procedure TJppColorControlRgbIntParams.PropsChanged(Sender: TObject);
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

//procedure TJppColorControlRgbIntParams.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

procedure TJppColorControlRgbIntParams.SetPaddingChar(const Value: Char);
begin
  FPaddingChar := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRgbIntParams.SetPaddingLen(const Value: integer);
begin
  FPaddingLen := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRgbIntParams.SetPrefix(const Value: string);
begin
  FPrefix := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRgbIntParams.SetRgbSeparator(const Value: string);
begin
  FRgbSeparator := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRgbIntParams.SetSuffix(const Value: string);
begin
  FSuffix := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorControlRgbIntParams}



{$Region '                        TJppColorControlRectangle                               '}

constructor TJppColorControlRectangle.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FVisible := True;
  FWidth := 40;
  FPaddingTop := 1;
  FPaddingBottom := 1;
  FBorderMode := bmAverageColor;
  FBorderWidth := 1;
  FBorderColor := clGray;
  FLeftMargin := 2;
  FHideTopBorder := False;
end;

destructor TJppColorControlRectangle.Destroy;
begin
  inherited;
end;

procedure TJppColorControlRectangle.Assign(ccr: TJppColorControlRectangle);
begin
  BeginUpdate;
  try
    Width := ccr.Width;
    PaddingTop := ccr.PaddingTop;
    PaddingBottom := ccr.PaddingBottom;
    BorderMode := ccr.BorderMode;
    BorderWidth := ccr.BorderWidth;
    BorderColor := ccr.BorderColor;
    LeftMargin := ccr.LeftMargin;
    HideTopBorder := ccr.HideTopBorder;
  finally
    EndUpdate;
  end;
end;

procedure TJppColorControlRectangle.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetBorderMode(const Value: TJppColorControlBorderMode);
begin
  if FBorderMode = Value then Exit;
  FBorderMode := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetBorderWidth(const Value: integer);
begin
  if FBorderWidth = Value then Exit;
  FBorderWidth := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetHideTopBorder(const Value: Boolean);
begin
  if FHideTopBorder = Value then Exit;
  FHideTopBorder := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetLeftMargin(const Value: integer);
begin
  if FLeftMargin = Value then Exit;
  FLeftMargin := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetPaddingBottom(const Value: integer);
begin
  if FPaddingBottom = Value then Exit;
  FPaddingBottom := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetPaddingTop(const Value: integer);
begin
  if FPaddingTop = Value then Exit;
  FPaddingTop := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlRectangle.SetWidth(const Value: integer);
begin
  if FWidth = Value then Exit;
  FWidth := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorControlRectangle}



{$Region '                       TJppColorControlAppearance                               '}

constructor TJppColorControlAppearance.Create(AOwner: TComponent);
var
  clB: TColor;
begin
  inherited Create;
  FOwner := AOwner;

  FUseCustomNumericFont := True;
  FNumericFont := TFont.Create;
  FNumericFont.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FNumericFont.Name := 'Consolas';
  FNumericFont.Size := 9;
  //FNumericFont.Style := [];

  FColorRectangle := TJppColorControlRectangle.Create(AOwner);
  FColorRectangle.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FRgbIntParams := TJppColorControlRgbIntParams.Create(AOwner);
  FRgbIntParams.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FRgbHexParams := TJppColorControlRgbHexParams.Create(AOwner);
  FRgbHexParams.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FBgrHexParams := TJppColorControlBgrHexParams.Create(AOwner);
  FBgrHexParams.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FShowColorName := True;
  FShowRgbInt := True;
  FShowRgbHex := False;
  FShowBgrHex := False;
  FDataSeparator := ' - ';
  FTextMargin := 6;
  FLeftMargin := 2;
  FTextPosDeltaY := -1;
  FNumericTextPosDeltaY := 0;
  FNumericFontSelectedColor := clHighlightText;

  FSeparatorItem := TJppColorControlSeparatorItem.Create(AOwner);
  FSeparatorItem.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FSelectedItem := TJppColorControlSelectedItem.Create(AOwner);
  FSelectedItem.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FSelectedItem.Background.Gradient.ColorFrom := clHighlight;
  FSelectedItem.Background.Gradient.ColorTo := $00FF8A15; //GetSimilarColor(clHighlight, 10, False);
  FSelectedItem.Background.Color := clHighlight;
  clB := $00FF860D;
  FSelectedItem.Background.Borders.Left.Color := clB;
  FSelectedItem.Background.Borders.Top.Color := clHighlight;
  FSelectedItem.Background.Borders.Right.Color := clB;
  FSelectedItem.Background.Borders.Bottom.Color := clB;

  FDisabledBackgroundColor := $00E9E9E9;
  FDisabledFontColor := clGray;
end;

destructor TJppColorControlAppearance.Destroy;
begin
  FRgbIntParams.Free;
  FRgbHexParams.Free;
  FBgrHexParams.Free;
  FColorRectangle.Free;
  FNumericFont.Free;
  FSeparatorItem.Free;
  FSelectedItem.Free;
  inherited;
end;

procedure TJppColorControlAppearance.Assign(cca: TJppColorControlAppearance);
begin
  BeginUpdate;
  try
    FColorRectangle.Assign(cca.ColorRectangle);
    FShowColorName := cca.ShowColorName;

    FShowRgbInt := cca.ShowRgbInt;
    FShowRgbHex := cca.ShowRgbHex;
    FShowBgrHex := cca.ShowBgrHex;

    FRgbHexParams.Assign(cca.RgbHexParams);
    FBgrHexParams.Assign(cca.BgrHexParams);
    FRgbIntParams.Assign(cca.RgbIntParams);

    FDataSeparator := cca.DataSeparator;
    FUseCustomNumericFont := cca.UseCustomNumericFont;
    FNumericFont.Assign(cca.NumericFont);
    FNumericFont.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
    FNumericFontSelectedColor := cca.NumericFontSelectedColor;
    FNumericTextPosDeltaY := cca.NumericTextPosDeltaY;

    FTextMargin := cca.TextMargin;
    FTextPosDeltaY := cca.TextPosDeltaY;

    FSeparatorItem.Assign(cca.SeparatorItem);
    FSeparatorItem.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

    FSelectedItem.Assign(cca.SelectedItem);
    FSelectedItem.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

    FDisabledBackgroundColor := cca.DisabledBackgroundColor;
    FDisabledFontColor := cca.DisabledFontColor;

  finally
    EndUpdate;
  end;
end;

//procedure TJppColorControlAppearance.PropsChanged(Sender: TObject);
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

procedure TJppColorControlAppearance.SetColorRectangle(const Value: TJppColorControlRectangle);
begin
  FColorRectangle := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetDataSeparator(const Value: string);
begin
  if FDataSeparator = Value then Exit;
  FDataSeparator := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetDisabledBackgroundColor(const Value: TColor);
begin
  if FDisabledBackgroundColor = Value then Exit;
  FDisabledBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetDisabledFontColor(const Value: TColor);
begin
  if FDisabledFontColor = Value then Exit;
  FDisabledFontColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetLeftMargin(const Value: integer);
begin
  if FLeftMargin = Value then Exit;
  FLeftMargin := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetNumericFont(const Value: TFont);
begin
  FNumericFont := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetNumericFontSelectedColor(const Value: TColor);
begin
  if FNumericFontSelectedColor = Value then Exit;
  FNumericFontSelectedColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetNumericTextPosDeltaY(const Value: integer);
begin
  if FNumericTextPosDeltaY = Value then Exit;
  FNumericTextPosDeltaY := Value;
  PropsChanged(Self);
end;

//procedure TJppColorControlAppearance.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

procedure TJppColorControlAppearance.SetBgrHexParams(const Value: TJppColorControlBgrHexParams);
begin
  FBgrHexParams := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetRgbHexParams(const Value: TJppColorControlRgbHexParams);
begin
  FRgbHexParams := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetRgbIntParams(const Value: TJppColorControlRgbIntParams);
begin
  FRgbIntParams := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetSelectedItem(const Value: TJppColorControlSelectedItem);
begin
  FSelectedItem := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetSeparatorItem(const Value: TJppColorControlSeparatorItem);
begin
  FSeparatorItem := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetShowBgrHex(const Value: Boolean);
begin
  if FShowBgrHex = Value then Exit;
  FShowBgrHex := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetShowColorName(const Value: Boolean);
begin
  if FShowColorName = Value then Exit;
  FShowColorName := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetShowRgbHex(const Value: Boolean);
begin
  if FShowRgbHex = Value then Exit;
  FShowRgbHex := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetShowRgbInt(const Value: Boolean);
begin
  if FShowRgbInt = Value then Exit;
  FShowRgbInt := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetTextMargin(const Value: integer);
begin
  if FTextMargin = Value then Exit;
  FTextMargin := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetTextPosDeltaY(const Value: integer);
begin
  if FTextPosDeltaY = Value then Exit;
  FTextPosDeltaY := Value;
  PropsChanged(Self);
end;

procedure TJppColorControlAppearance.SetUseCustomNumericFont(const Value: Boolean);
begin
  if FUseCustomNumericFont = Value then Exit;
  FUseCustomNumericFont := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorControlAppearance}




end.

