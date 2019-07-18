unit JPP.BasicSpeedButton;

{
  ---------------------------------------------------------------------------------------
  Based on PngSpeedButton from PngComponents https://bitbucket.org/uweraabe/pngcomponents
  ---------------------------------------------------------------------------------------
  JP
  2019.05.21
  [+] AutoWidth
}

interface

uses
  {$IFDEF DCC}
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Graphics,  Vcl.StdCtrls, Vcl.Buttons, Vcl.GraphUtil, Vcl.ActnList, Vcl.Imaging.pngimage,
  PngFunctions, PngButtonFunctions, PngImageList,
  {$ELSE}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, Controls, Graphics, StdCtrls, ActnList, Buttons, LCLType, LCLIntf, LMessages, Messages,
  {$ENDIF}

  JPL.Colors,
  JPP.Types, JPP.Graphics, JPP.Common, JPP.Common.Procs
  ,LDPngFunctions
  ;

type


  TJppBasicSpeedButtonState = (bssNormal, bssOver, bssDown, bssDisabled);



  {$region ' ------------ TJppBasicSpeedButtonStateParams ------------ '}
  TJppBasicSpeedButtonStateParams = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FFontColor: TColor;
    FBorderColor: TColor;
    FBorderWidth: integer;
    FTransparentBorder: Boolean;
    FTransparentBackground: Boolean;
    FColor: TColor;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetFontColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: integer);
    procedure SetTransparentBorder(const Value: Boolean);
    procedure SetTransparentBackground(const Value: Boolean);
    procedure SetColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property Color: TColor read FColor write SetColor; // default clBtnFace;
    property FontColor: TColor read FFontColor write SetFontColor; // default clWindowText;
    property BorderColor: TColor read FBorderColor write SetBorderColor; // default clWindowFrame;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth; // default 1;
    property TransparentBorder: Boolean read FTransparentBorder write SetTransparentBorder default False;
    property TransparentBackground: Boolean read FTransparentBackground write SetTransparentBackground default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' ---------- TJppBasicSpeedButtonAppearance ------------- '}
  TJppBasicSpeedButtonAppearance = class(TPersistent)
  private
    FGlyphDisabledBlendIntensity: TJppBlendIntensity;
    FGlyphDisabledGrayscaleIntensity: TJppGrayscaleIntensity;
    FOnChange: TNotifyEvent;
    FNormal: TJppBasicSpeedButtonStateParams;
    FHot: TJppBasicSpeedButtonStateParams;
    FDown: TJppBasicSpeedButtonStateParams;
    FDisabled: TJppBasicSpeedButtonStateParams;
    //FGlyphDisabledGrayscaleFactor: Byte;
    //FGlyphDisabledBlendFactor: Byte;
    FGlyphHotGammaFactor: Byte;
    FMoveWhenDown: Boolean;
    FShowCaption: Boolean;
    procedure SetGlyphDisabledBlendIntensity(AValue: TJppBlendIntensity);
    procedure SetGlyphDisabledGrayscaleIntensity(AValue: TJppGrayscaleIntensity);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetNormal(const Value: TJppBasicSpeedButtonStateParams);
    procedure SetHot(const Value: TJppBasicSpeedButtonStateParams);
    procedure SetDown(const Value: TJppBasicSpeedButtonStateParams);
    procedure SetDisabled(const Value: TJppBasicSpeedButtonStateParams);
    //procedure SetGlyphDisabledGrayscaleFactor(const Value: Byte);
    //procedure SetGlyphDisabledBlendFactor(const Value: Byte);
    procedure SetGlyphHotGammaFactor(const Value: Byte);
    procedure SetMoveWhenDown(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(A: TJppBasicSpeedButtonAppearance; bIgnoreShowCaption: Boolean = False); reintroduce;
  published
    property Normal: TJppBasicSpeedButtonStateParams read FNormal write SetNormal;
    property Hot: TJppBasicSpeedButtonStateParams read FHot write SetHot;
    property Down: TJppBasicSpeedButtonStateParams read FDown write SetDown;
    property Disabled: TJppBasicSpeedButtonStateParams read FDisabled write SetDisabled;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    //property GlyphDisabledGrayscaleFactor: Byte read FGlyphDisabledGrayscaleFactor write SetGlyphDisabledGrayscaleFactor default 255;
    //property GlyphDisabledBlendFactor: Byte read FGlyphDisabledBlendFactor write SetGlyphDisabledBlendFactor default 127;
    property GlyphHotGammaFactor: Byte read FGlyphHotGammaFactor write SetGlyphHotGammaFactor default 130;
    property MoveWhenDown: Boolean read FMoveWhenDown write SetMoveWhenDown default False;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property GlyphDisabledBlendIntensity: TJppBlendIntensity read FGlyphDisabledBlendIntensity write SetGlyphDisabledBlendIntensity default biNormal;
    property GlyphDisabledGrayscaleIntensity: TJppGrayscaleIntensity read FGlyphDisabledGrayscaleIntensity write SetGlyphDisabledGrayscaleIntensity
      default gi90Percent;
  end;
  {$endregion}

  {$region ' ----------------- TJppBasicSpeedButton ----------------- '}
  TJppBasicSpeedButton = class(TGraphicControl)
  private
    FTagExt: TJppTagExt;
    FButtonState: TJppBasicSpeedButtonState;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FCaption: string;
    FMouseInControl: Boolean;
    FAppearance: TJppBasicSpeedButtonAppearance;
    FImageFromAction: Boolean;
    FPngImage: TPngImage;
    FPngOptions: TPngOptions;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FSpacing: Integer;
    FAutoWidth: Boolean;
    FAutoWidthMargin: ShortInt;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure SetCaption(const Value: string);
    procedure SetAppearance(const Value: TJppBasicSpeedButtonAppearance);
    function PngImageStored: Boolean;
    procedure SetPngImage(const Value: TPngImage);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetAutoWidthMargin(const Value: ShortInt);
  protected
    procedure Paint; override;
    property ButtonState: TJppBasicSpeedButtonState read FButtonState;
    property MouseInControl: Boolean read FMouseInControl;
    procedure Loaded; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure PropsChanged(Sender: TObject);
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function CaptionWidth: integer;
  published
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    property Caption: string read FCaption write SetCaption;
    property Appearance: TJppBasicSpeedButtonAppearance read FAppearance write SetAppearance;
    property PngImage: TPngImage read FPngImage write SetPngImage stored PngImageStored;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled, pngGrayscaleOnDisabled];
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default True;
    property AutoWidthMargin: ShortInt read FAutoWidthMargin write SetAutoWidthMargin default 6;

    property Action;
    property Align;
    {$IFDEF DCC}property AlignWithMargins;{$ENDIF}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Cursor;
    property Enabled;
    property Font;
    {$IFDEF DCC}property Margins;{$ENDIF}
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF DCC}{$IF RTLVersion > 23} property StyleElements; {$IFEND}{$ENDIF}
    property Visible;

    property OnClick;
    property OnDblClick;
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF FPC}
    property BorderSpacing;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnChangeBounds;
    {$ENDIF}

  end;
  {$endregion}



implementation



{$region ' --------------------- TJppBasicSpeedButton --------------------------- '}

constructor TJppBasicSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csDoubleClicks];

  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppBasicSpeedButtonAppearance.Create(Self);
  FAppearance.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FButtonState := bssNormal;
  FPngImage := TPngImage.Create;
  FPngOptions := [pngBlendOnDisabled, pngGrayscaleOnDisabled];

  FMargin := -1;
  FSpacing := 4;

  Width := 75;
  Height := 25;

  FAutoWidth := True;
  FAutoWidthMargin := 6;

  //Repaint;
end;


destructor TJppBasicSpeedButton.Destroy;
begin
  FTagExt.Free;
  FAppearance.Free;
  FPngImage.Free;
  inherited;
end;

procedure TJppBasicSpeedButton.Loaded;
begin
  inherited Loaded;
  if Enabled then FButtonState := bssNormal else FButtonState := bssDisabled;
end;


procedure TJppBasicSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;
  if Enabled then FButtonState := bssOver else FButtonState := bssDisabled;
  if csDesigning in ComponentState then Exit;
  if Enabled then
    if Assigned(FOnMouseEnter) then OnMouseEnter(Self);
  Repaint;
end;

procedure TJppBasicSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;
  if Enabled then FButtonState := bssNormal else FButtonState := bssDisabled;
  if csDesigning in ComponentState then Exit;
  if Enabled then
    if Assigned(FOnMouseLeave) then OnMouseLeave(Self);
  Repaint;
end;

procedure TJppBasicSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
    if ButtonState <> bssDown then
    begin
      FButtonState := bssDown;
      Repaint;
    end;
end;

procedure TJppBasicSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CanClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Enabled then
  begin
    FButtonState := bssOver;
    //FButtonState := bssNormal;
    CanClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if CanClick then Click;
    Repaint;
  end;

end;

procedure TJppBasicSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);

  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      //Copy image from action's imagelist
      Self.Caption := Caption;
      Self.Hint := Hint;
      Self.ShowHint := ShowHint;
      if (pngimage.Empty or FImageFromAction) and (ActionList <> nil) and (ActionList.Images <> nil) and (ImageIndex >= 0) and
        (ImageIndex < ActionList.Images.Count) then
      begin
        CopyImageFromImageList(FPngImage, ActionList.Images, ImageIndex);
        FImageFromAction := True;
      end;
    end;
  end;

  {$IFDEF LINUX}
  // Force full repaint. Repaint method sometimes not working
  if Visible then
  begin
    Repaint;
    Width := Width + 1;
    Width := Width - 1;
    //Repaint;
  end;
  {$ENDIF}
end;

function TJppBasicSpeedButton.CaptionWidth: integer;
begin
  Result := Canvas.TextWidth(Caption);
end;

procedure TJppBasicSpeedButton.Click;
begin
  inherited Click;
  //if Assigned(OnClick) then OnClick(Self);
end;

procedure TJppBasicSpeedButton.CMEnabledChanged(var Message: TMessage);
begin
  if not Enabled then FButtonState := bssDisabled else FButtonState := bssNormal;
  Repaint;
end;



procedure TJppBasicSpeedButton.Paint;

type
  TDrawingParams = record
    Color: TColor;
    Font: TFont;
    FontColor: TColor;
    BorderColor: TColor;
    BorderWidth: integer;
    TransparentBackground: Boolean;
    TransparentBorder: Boolean;
  end;

var
  R: TRect;
  dp: TDrawingParams;
  PaintRect: TRect;
  GlyphPos, TextPos: TPoint;
  bOver, bDown: Boolean;
  imgDisabled, imgHot: TPngImage;
  s: string;
  ButtonWidth: integer;

  procedure CopyDrawingParams(bsp: TJppBasicSpeedButtonStateParams);
  begin
    dp.Color := bsp.Color;
    dp.FontColor := bsp.FontColor;
    dp.BorderColor := bsp.BorderColor;
    dp.BorderWidth := bsp.BorderWidth;
    dp.TransparentBackground := bsp.TransparentBackground;
    dp.TransparentBorder := bsp.TransparentBorder;
  end;

begin
  //inherited;
  ButtonWidth := 0;

  R := ClientRect;
  with Canvas do
  begin

    dp.Font := Self.Font;
    Canvas.Font := dp.Font;

    if ButtonState = bssDisabled then
    begin
      CopyDrawingParams(Appearance.Disabled);
    end

    else if ButtonState = bssOver then
    begin
      CopyDrawingParams(Appearance.Hot);
    end

    else if ButtonState = bssDown then
    begin
      CopyDrawingParams(Appearance.Down);
    end

    else

    // Normal
    begin
      CopyDrawingParams(Appearance.Normal);
    end;


    // ---------------------------- BACKGROUND ------------------------
    if not dp.TransparentBackground then
    begin
      Brush.Color := dp.Color;
      Brush.Style := bsSolid;
      FillRect(R);
    end;

    // ----------------------- BORDER ---------------------------
    if not dp.TransparentBorder then
    begin
      Brush.Style := bsClear;
      Pen.Color := dp.Color;
      JppFrame3D(Canvas, R, dp.BorderColor, dp.BorderWidth);
    end;



    // --------------------------------------------- IMAGE ------------------------------------------------------
    bOver := ButtonState = bssOver;
    bDown := ButtonState = bssDown;
    Canvas.Font := dp.Font; // <-- potrzebne aby dopasowaæ pozycjê obrazka i tekstu
    if Appearance.ShowCaption then s := Caption else s := '';


    CalcButtonLayout(
      Canvas, FPngImage, ClientRect, bDown and Appearance.MoveWhenDown, False, s, Layout, Margin, Spacing, GlyphPos, TextPos,
      {$IFDEF DCC}DrawTextBiDiModeFlags(0){$ELSE}0{$ENDIF}
    );

    if (FPngImage <> nil) {and (Kind = bkCustom)} and not FPngImage.Empty then
    begin
      PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, FPngImage.Width, FPngImage.Height);

      if Enabled and (bOver or bDown) then
      begin
        imgHot := TPngImage.Create;
        try
          imgHot.Assign(FPngImage);
          //if PngGetCopy(FPngImage, imgHot) then
          begin
            PngSetGamma(imgHot, Appearance.GlyphHotGammaFactor / 100);
            //PngSetBrightness(imgHot, 20);
            Draw(GlyphPos.X, GlyphPos.Y, imgHot);
          end;
        finally
          imgHot.Free;
        end;
      end

      else if Enabled then Draw(GlyphPos.X, GlyphPos.Y, FPngImage)

      else
      // disabled
      begin
        imgDisabled := TPngImage.Create;
        try
          imgDisabled.Assign(FPngImage);
          //if pngGrayscaleOnDisabled in FPngOptions then
          //MakeImageGrayscale(imgDisabled, 179);
          //MakeImageBlended(imgDisabled, 220);
          //if pngGrayscaleOnDisabled in FPngOptions then MakeImageGrayscale(imgDisabled, Appearance.GlyphDisabledGrayscaleFactor);
          //if pngBlendOnDisabled in FPngOptions then MakeImageBlended(imgDisabled, Appearance.GlyphDisabledBlendFactor);
          MakeDisabledImage(
            imgDisabled,
            FAppearance.GlyphDisabledBlendIntensity, FAppearance.GlyphDisabledGrayscaleIntensity,
            (pngBlendOnDisabled in FPngOptions), (pngGrayscaleOnDisabled in FPngOptions)
          );
          Draw(GlyphPos.X, GlyphPos.Y, imgDisabled);
        finally
          imgDisabled.Free;
        end;
      end;

    end;

    if Assigned(PngImage) and (not PngImage.Empty) then Inc(ButtonWidth, PngImage.Width);
    if FCaption <> '' then Inc(ButtonWidth, FSpacing);


    // ---------------------- TEXT ------------------------------
//    if Caption <> '' then
//    begin
//      Brush.Style := bsClear;
//      Canvas.Font.Assign(Self.Font);
//      Canvas.Font.Color := dp.FontColor;
//      InflateRect(R, dp.BorderWidth, dp.BorderWidth);
//      xtLeft := (R.Width div 2) - (TextWidth(Caption) div 2);
//      xtTop := (R.Height div 2) - (TextHeight(Caption) div 2);
//      //TextOut(xtLeft, xtTop, Caption);
//      R.Left := xtLeft;
//      R.Top := xtTop;
//      DrawText(Canvas.Handle, PChar(Caption), -1, R, DrawTextBiDiModeFlags(0) or DT_TOP or DT_LEFT or DT_SINGLELINE);
//    end;



    if Appearance.ShowCaption and (Length(Caption) > 0) then
    begin
      Brush.Style := bsClear;
      Canvas.Font.Assign(Self.Font);
      Canvas.Font.Color := dp.FontColor;


      if FAutoWidth then
      begin
        Inc(ButtonWidth, Canvas.TextWidth(Caption));
        Inc(ButtonWidth, FAutoWidthMargin + FAutoWidthMargin);
        if FMargin > 0 then Inc(ButtonWidth, FMargin);
        if ButtonWidth < 16 then ButtonWidth := 16;
        Self.Width := ButtonWidth;
      end;

      PaintRect := Rect(TextPos.X, TextPos.Y, Width, Height);
      //Canvas.Brush.Style := bsClear;
      DrawText(
        Canvas.Handle, PChar(Caption), -1, PaintRect,
        {$IFDEF DCC}DrawTextBiDiModeFlags(0) or {$ENDIF} DT_TOP or DT_LEFT or DT_SINGLELINE
      );
    end;

  end; // with Canvas
end;

function TJppBasicSpeedButton.PngImageStored: Boolean;
begin
  Result := not FImageFromAction;
end;

procedure TJppBasicSpeedButton.PropsChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TJppBasicSpeedButton.SetAppearance(const Value: TJppBasicSpeedButtonAppearance);
begin
  FAppearance := Value;
  Repaint;
end;

procedure TJppBasicSpeedButton.SetAutoWidth(const Value: Boolean);
begin
  if FAutoWidth = Value then Exit;
  FAutoWidth := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButton.SetAutoWidthMargin(const Value: ShortInt);
begin
  if FAutoWidthMargin = Value then Exit;
  FAutoWidthMargin := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButton.SetCaption(const Value: string);
begin
  FCaption := Value;
  PropsChanged(Self);
end;


procedure TJppBasicSpeedButton.SetLayout(const Value: TButtonLayout);
begin
  FLayout := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButton.SetMargin(const Value: Integer);
begin
  FMargin := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButton.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  FOnMouseEnter := Value;
end;

procedure TJppBasicSpeedButton.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  FOnMouseLeave := Value;
end;

procedure TJppBasicSpeedButton.SetPngImage(const Value: TPngImage);
begin
  //This is all neccesary, because you can't assign a nil to a TPngImage
  if Value = nil then
  begin
    FPngImage.Free;
    FPngImage := TPngImage.Create;
  end
  else
  begin
    FPngImage.Assign(Value);
  end;

  {$IFDEF DCC}
  //To work around the gamma-problem
  with FPngImage do
    if not Empty and (Header.ColorType in [COLOR_RGB, COLOR_RGBALPHA, COLOR_PALETTE]) then Chunks.RemoveChunk(Chunks.ItemFromClass(TChunkgAMA));
  {$ENDIF}

  FImageFromAction := False;
  Repaint;
end;

procedure TJppBasicSpeedButton.SetPngOptions(const Value: TPngOptions);
begin
  if FPngOptions <> Value then
  begin
    FPngOptions := Value;
    Repaint;
  end;
end;

procedure TJppBasicSpeedButton.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButton.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

{$endregion}


{$region ' ------------------------- TJppBasicSpeedButtonStateParams ---------------------- '}
constructor TJppBasicSpeedButtonStateParams.Create(AOwner: TComponent);
begin
  inherited Create;
//  FColor := clBtnFace;
//  FFontColor := clWindowText;
//  FBorderColor := clWindowFrame;
//  FBorderWidth := 1;
//  FTransparentBorder := False;
//  FTransparentBackground := False;
end;

destructor TJppBasicSpeedButtonStateParams.Destroy;
begin
  inherited;
end;

procedure TJppBasicSpeedButtonStateParams.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicSpeedButtonStateParams.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonStateParams.SetBorderWidth(const Value: integer);
begin
  FBorderWidth := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonStateParams.SetColor(const Value: TColor);
begin
  FColor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonStateParams.SetFontColor(const Value: TColor);
begin
  FFontColor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonStateParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppBasicSpeedButtonStateParams.SetTransparentBackground(const Value: Boolean);
begin
  FTransparentBackground := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonStateParams.SetTransparentBorder(const Value: Boolean);
begin
  FTransparentBorder := Value;
  PropsChanged(Self);
end;

{$endregion TJppBasicSpeedButtonStateParams}


{$region ' ---------------------- TJppBasicSpeedButtonAppearance ------------------------ '}


constructor TJppBasicSpeedButtonAppearance.Create(AOwner: TComponent);
begin
  FNormal := TJppBasicSpeedButtonStateParams.Create(AOwner);
  FHot := TJppBasicSpeedButtonStateParams.Create(AOwner);
  FDown := TJppBasicSpeedButtonStateParams.Create(AOwner);
  FDisabled := TJppBasicSpeedButtonStateParams.Create(AOwner);


  FNormal.Color := clBtnFace;
  FNormal.FontColor := clWindowText;
  FNormal.BorderColor := clWindowFrame;
  FNormal.BorderWidth := 1;
  FNormal.TransparentBorder := False;
  FNormal.TransparentBackground := False;

  FHot.Color := clHighlight;
  FHot.FontColor := clHighlightText;
  FHot.BorderColor := clHighlight;
  FHot.BorderWidth := 1;
  FHot.TransparentBorder := False;
  FHot.TransparentBackground := False;

  FDown.Color := GetSimilarColor(FHot.Color, 30, False);
  FDown.FontColor := FHot.FontColor;
  FDown.BorderColor := FDown.Color; // FHot.Color;
  FDown.BorderWidth := 1;
  FDown.TransparentBorder := False;
  FDown.TransparentBackground := False;

  FDisabled.Color := GetSimilarColor(clBtnFace, 3, True);
  FDisabled.FontColor := clBtnShadow;
  FDisabled.BorderColor := FDisabled.FontColor;
  FDisabled.BorderWidth := 1;
  FDisabled.TransparentBorder := False;
  FDisabled.TransparentBackground := False;

  FNormal.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FHot.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FDown.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FDisabled.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  //FGlyphDisabledGrayscaleFactor := 255;
  //FGlyphDisabledBlendFactor := 127;
  FGlyphHotGammaFactor := 130;

  FGlyphDisabledBlendIntensity := biNormal;
  FGlyphDisabledGrayscaleIntensity := gi90Percent;

  FShowCaption := True;
end;

destructor TJppBasicSpeedButtonAppearance.Destroy;
begin
  FNormal.Free;
  FHot.Free;
  FDown.Free;
  FDisabled.Free;
  inherited;
end;

procedure TJppBasicSpeedButtonAppearance.Assign(A: TJppBasicSpeedButtonAppearance; bIgnoreShowCaption: Boolean = False);
begin
  Normal.BorderColor := A.Normal.BorderColor;
  Normal.BorderWidth := A.Normal.BorderWidth;
  Normal.Color := A.Normal.Color;
  Normal.FontColor := A.Normal.FontColor;
  Normal.TransparentBackground := A.Normal.TransparentBackground;
  Normal.TransparentBorder := A.Normal.TransparentBorder;

  Hot.BorderColor := A.Hot.BorderColor;
  Hot.BorderWidth := A.Hot.BorderWidth;
  Hot.Color := A.Hot.Color;
  Hot.FontColor := A.Hot.FontColor;
  Hot.TransparentBackground := A.Hot.TransparentBackground;
  Hot.TransparentBorder := A.Hot.TransparentBorder;

  Down.BorderColor := A.Down.BorderColor;
  Down.BorderWidth := A.Down.BorderWidth;
  Down.Color := A.Down.Color;
  Down.FontColor := A.Down.FontColor;
  Down.TransparentBackground := A.Down.TransparentBackground;
  Down.TransparentBorder := A.Down.TransparentBorder;

  Disabled.BorderColor := A.Disabled.BorderColor;
  Disabled.BorderWidth := A.Disabled.BorderWidth;
  Disabled.Color := A.Disabled.Color;
  Disabled.FontColor := A.Disabled.FontColor;
  Disabled.TransparentBackground := A.Disabled.TransparentBackground;
  Disabled.TransparentBorder := A.Disabled.TransparentBorder;

  MoveWhenDown := A.MoveWhenDown;
  //GlyphDisabledGrayscaleFactor := A.GlyphDisabledGrayscaleFactor;
  //GlyphDisabledBlendFactor := A.GlyphDisabledBlendFactor;
  GlyphHotGammaFactor := A.GlyphHotGammaFactor;

  FGlyphDisabledBlendIntensity := A.GlyphDisabledBlendIntensity;
  FGlyphDisabledGrayscaleIntensity := A.GlyphDisabledGrayscaleIntensity;

  if not bIgnoreShowCaption then ShowCaption := A.ShowCaption;
end;

procedure TJppBasicSpeedButtonAppearance.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetDisabled(const Value: TJppBasicSpeedButtonStateParams);
begin
  FDisabled := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetDown(const Value: TJppBasicSpeedButtonStateParams);
begin
  FDown := Value;
  PropsChanged(Self);
end;

//procedure TJppBasicSpeedButtonAppearance.SetGlyphDisabledBlendFactor(const Value: Byte);
//begin
//  FGlyphDisabledBlendFactor := Value;
//  PropsChanged(Self);
//end;

//procedure TJppBasicSpeedButtonAppearance.SetGlyphDisabledGrayscaleFactor(const Value: Byte);
//begin
//  FGlyphDisabledGrayscaleFactor := Value;
//  PropsChanged(Self);
//end;
//
procedure TJppBasicSpeedButtonAppearance.SetGlyphHotGammaFactor(const Value: Byte);
begin
  FGlyphHotGammaFactor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetNormal(const Value: TJppBasicSpeedButtonStateParams);
begin
  FNormal := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppBasicSpeedButtonAppearance.SetGlyphDisabledBlendIntensity(AValue: TJppBlendIntensity);
begin
  if FGlyphDisabledBlendIntensity = AValue then Exit;
  FGlyphDisabledBlendIntensity := AValue;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetGlyphDisabledGrayscaleIntensity(AValue: TJppGrayscaleIntensity);
begin
  if FGlyphDisabledGrayscaleIntensity = AValue then Exit;
  FGlyphDisabledGrayscaleIntensity := AValue;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetHot(const Value: TJppBasicSpeedButtonStateParams);
begin
  FHot := Value;
  PropsChanged(Self);
end;

procedure TJppBasicSpeedButtonAppearance.SetMoveWhenDown(const Value: Boolean);
begin
  FMoveWhenDown := Value;
  PropsChanged(Self);
end;

{$endregion TJppBasicSpeedButtonAppearance}

end.
