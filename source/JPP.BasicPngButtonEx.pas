unit JPP.BasicPngButtonEx;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  Based on the TPngBitBtn from then PngComponents package: https://github.com/UweRaabe/PngComponents
  PngComponents license: https://github.com/UweRaabe/PngComponents/blob/master/Docs/License.txt

  My modifications: public domain
}

{$I jpp.inc}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

{$IFDEF MSWINDOWS}

uses
  {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF}
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  Classes, SysUtils, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF} StrUtils,
  Graphics, Controls, Buttons, GraphUtil, Dialogs, StdCtrls,
  {$IFDEF DCC}{$IFDEF HAS_UNIT_SCOPE}Vcl.Imaging.pngimage,{$ELSE}pngimage,{$ENDIF}{$ENDIF}
  {$IFDEF DCC}PngFunctions,{$ENDIF}

  LDPngFunctions,
  JPL.Strings, JPL.Colors, JPL.Rects,
  JPP.Types, JPP.Common, JPP.Common.Procs, JPP.Helpers, JPP.AnchoredControls, JPP.Gradient, JPP.Graphics
  ;

type


  {$region ' -------------- TJppBasicPngButtonExStateParams --------------------- '}
  TJppBasicPngButtonExStateParams = class(TPersistent)
  private
    FColor: TColor;
    FFont: TFont;
    FBorder: TPen;
    FGradientEnabled: Boolean;
    FGradient: TJppGradientEx;
    FBorderToGradientMargin: integer;
    FTransparentBackground: Boolean;
    FTransparentFrame: Boolean;
    FOnChange: TNotifyEvent;
    FSubCaptionColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetBorder(const Value: TPen);
    procedure SetGradientEnabled(const Value: Boolean);
    procedure SetGradient(const Value: TJppGradientEx);
    procedure SetBorderToGradientMargin(const Value: integer);
    procedure SetTransparentBackground(const Value: Boolean);
    procedure SetTransparentFrame(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetSubCaptionColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property Border: TPen read FBorder write SetBorder;
    property Color: TColor read FColor write SetColor;
    property Font: TFont read FFont write SetFont;
    property Gradient: TJppGradientEx read FGradient write SetGradient;
    property GradientEnabled: Boolean read FGradientEnabled write SetGradientEnabled default True;
    property BorderToGradientMargin: integer read FBorderToGradientMargin write SetBorderToGradientMargin default 2;
    property TransparentBackground: Boolean read FTransparentBackground write SetTransparentBackground default False;
    property TransparentFrame: Boolean read FTransparentFrame write SetTransparentFrame default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property SubCaptionColor: TColor read FSubCaptionColor write SetSubCaptionColor default clWindowText;
  end;
  {$endregion}


  {$region ' ----------- TJppBasicPngButtonExAppearance ---------- '}
  TJppBasicPngButtonExAppearance = class(TPersistent)
  private
    FNormal: TJppBasicPngButtonExStateParams;
    FHot: TJppBasicPngButtonExStateParams;
    FDefaultDrawing: Boolean;
    FFocusRect: TJppFocusRectParams;
    FBorderWhenDefault: TPen;
    FDown: TJppBasicPngButtonExStateParams;
    FDisabled: TJppBasicPngButtonExStateParams;
    FGlyphDisabledGrayscaleFactor: Byte;
    FGlyphDisabledBlendFactor: Byte;
    FMoveWhenDown: Boolean;
    FGlyphHotGammaFactor: Byte;
    FOnChange: TNotifyEvent;
    FFocused: TJppBasicPngButtonExStateParams;
    FShowCaption: Boolean;
    FSubCaptionFont: TFont;
    FSubCaption: string;
    FCaptionsMargin: integer;
    FSubCaptionEllipsis: TEllipsisPosition;
    FCaptionAlignment: TAlignment;
    FCaptionEllipsis: TEllipsisPosition;
    procedure SetNormal(const Value: TJppBasicPngButtonExStateParams);
    procedure SetHot(const Value: TJppBasicPngButtonExStateParams);
    procedure SetDefaultDrawing(const Value: Boolean);
    procedure SetFocusRect(const Value: TJppFocusRectParams);
    procedure SetBorderWhenDefault(const Value: TPen);
    procedure SetDown(const Value: TJppBasicPngButtonExStateParams);
    procedure SetDisabled(const Value: TJppBasicPngButtonExStateParams);
    procedure SetGlyphDisabledGrayscaleFactor(const Value: Byte);
    procedure SetGlyphDisabledBlendFactor(const Value: Byte);
    procedure SetMoveWhenDown(const Value: Boolean);
    procedure SetGlyphHotGammaFactor(const Value: Byte);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetFocused(const Value: TJppBasicPngButtonExStateParams);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetSubCaptionFont(const Value: TFont);
    procedure SetSubCaption(const Value: string);
    procedure SetCaptionsMargin(const Value: integer);
    procedure SetSubCaptionEllipsis(const Value: TEllipsisPosition);
    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetCaptionEllipsis(const Value: TEllipsisPosition);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppBasicPngButtonExAppearance); reintroduce;
    procedure PropsChanged(Sender: TObject);
  published
    property DefaultDrawing: Boolean read FDefaultDrawing write SetDefaultDrawing default False;
    property Normal: TJppBasicPngButtonExStateParams read FNormal write SetNormal;
    property Hot: TJppBasicPngButtonExStateParams read FHot write SetHot;
    property Down: TJppBasicPngButtonExStateParams read FDown write SetDown;
    property Disabled: TJppBasicPngButtonExStateParams read FDisabled write SetDisabled;
    property Focused: TJppBasicPngButtonExStateParams read FFocused write SetFocused;
    property FocusRect: TJppFocusRectParams read FFocusRect write SetFocusRect;
    property BorderWhenDefault: TPen read FBorderWhenDefault write SetBorderWhenDefault;
    property GlyphDisabledGrayscaleFactor: Byte read FGlyphDisabledGrayscaleFactor write SetGlyphDisabledGrayscaleFactor default 255;
    property GlyphDisabledBlendFactor: Byte read FGlyphDisabledBlendFactor write SetGlyphDisabledBlendFactor default 127;
    property GlyphHotGammaFactor: Byte read FGlyphHotGammaFactor write SetGlyphHotGammaFactor default 130;
    property MoveWhenDown: Boolean read FMoveWhenDown write SetMoveWhenDown default False;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property SubCaptionFont: TFont read FSubCaptionFont write SetSubCaptionFont;
    property SubCaption: string read FSubCaption write SetSubCaption;
    property CaptionsMargin: integer read FCaptionsMargin write SetCaptionsMargin default 2;
    property SubCaptionEllipsis: TEllipsisPosition read FSubCaptionEllipsis write SetSubCaptionEllipsis default epEndEllipsis;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taCenter;
    property CaptionEllipsis: TEllipsisPosition read FCaptionEllipsis write SetCaptionEllipsis default epNone;
  end;
  {$endregion}


  {$region ' -------------- TJppBasicPngButtonEx -------------- '}
  TJppBasicPngButtonEx = class(TBitBtn)
  {$IFDEF DCC}
  {$IF RTLVersion >= 24.0 }
  strict private
    class constructor Create;
    class destructor Destroy;
  {$IFEND}
  {$ENDIF}
  private
    bOver: Boolean;
    FPngImage: TPngImage;
    FPngOptions: TPngOptions;
    FCanvas: TCanvas;
    FLastKind: TBitBtnKind;
    FImageFromAction: Boolean;
    {$IFDEF DELPHIXE2_OR_ABOVE}FMouseInControl: Boolean;{$ENDIF}
    IsFocused: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FTagExt: TJppTagExt;
    FAppearance: TJppBasicPngButtonExAppearance;
    FAnchoredControls: TJppAnchoredControls;
    function PngImageStored: Boolean;
    procedure SetPngImage(const Value: TPngImage);
    procedure SetPngOptions(const Value: TPngOptions);
    {$IFDEF DCC}
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    {$ELSE}
    procedure CNDrawItem(var Message: TLMDrawItems); message CN_DRAWITEM;
    {$ENDIF}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAppearance(const Value: TJppBasicPngButtonExAppearance);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    {$IFDEF DCC}
    procedure SetButtonStyle(ADefault: Boolean); override;
    {$ENDIF}
    procedure PropsChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {$IFDEF FPC}
    function DrawTextBiDiModeFlags(Flags: Longint): Longint;
    procedure WndProc(var TheMessage : TLMessage); override;
    procedure LMDrawItem(var Message: TLMDrawItems); message LM_DRAWITEM;
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property PngImage: TPngImage read FPngImage write SetPngImage stored PngImageStored;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property Appearance: TJppBasicPngButtonExAppearance read FAppearance write SetAppearance;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppBasicPngButtonEx}


  {$IFDEF DCC}
  {$IF RTLVersion >= 24.0 }
  TJppBasicPngButtonExStyleHook = class(TBitBtnStyleHook)
  strict protected
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
  end;
  {$IFEND}
  {$ENDIF}




{$IFDEF FPC}
function DrawCtrlTextBiDiModeFlags(AControl: TControl; Flags: Longint): Longint;
{$ENDIF}
procedure SetJppBasicPngButtonExFonts(Button: TJppBasicPngButtonEx; FontName: string = 'Segoe UI'; FontSize: integer = 9);


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}

uses
  ActnList {$IFDEF DCC}, Themes, PngButtonFunctions, PngImageList{$ENDIF};



{$region ' -------------- helpers ------------ '}

{$IFDEF FPC}
function DrawCtrlTextBiDiModeFlags(AControl: TControl; Flags: Longint): Longint;
var
  Alignment: TAlignment;
begin
  Result := Flags;
  if AControl.UseRightToLeftAlignment then
  begin
    if Flags and DT_RIGHT <> 0 then Alignment := taRightJustify
    else if Flags and DT_CENTER <> 0 then Alignment := taCenter
    else Alignment := taLeftJustify;

    case Alignment of
      taLeftJustify: Result := Result or DT_RIGHT;
      taRightJustify: Result := Result and not DT_RIGHT;
    end;
  end;

  if AControl.UseRightToLeftReading then Result := Result or DT_RTLREADING;
end;
{$ENDIF}

procedure SetJppBasicPngButtonExFonts(Button: TJppBasicPngButtonEx; FontName: string = 'Segoe UI'; FontSize: integer = 9);
begin
  Button.Font.Name := FontName;
  Button.Font.Size := FontSize;
  Button.Appearance.Normal.Font.Name := FontName;
  Button.Appearance.Normal.Font.Size := FontSize;
  Button.Appearance.Hot.Font.Name := FontName;
  Button.Appearance.Hot.Font.Size := FontSize;
  Button.Appearance.Down.Font.Name := FontName;
  Button.Appearance.Down.Font.Size := FontSize;
  Button.Appearance.Focused.Font.Name := FontName;
  Button.Appearance.Focused.Font.Size := FontSize;
  Button.Appearance.Disabled.Font.Name := FontName;
  Button.Appearance.Disabled.Font.Size := FontSize;
end;

{$endregion}


{$region ' -------------------------------- TJppBasicPngButtonEx ------------------------------------- '}


  {$region ' ---------------------- Cretae & Destroy ----------------------------- '}
constructor TJppBasicPngButtonEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPngImage := TPngImage.Create;
  FPngOptions := [pngBlendOnDisabled];
  FCanvas := TCanvas.Create;
  FLastKind := bkCustom;
  FImageFromAction := False;
  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppBasicPngButtonExAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;
  bOver := False;
  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppBasicPngButtonEx.Destroy;
begin
  FPngImage.Free;
  FCanvas.Free;
  FTagExt.Free;
  FAppearance.Free;
  FAnchoredControls.Free;
  inherited Destroy;
end;


{$endregion Create & Destroy}


procedure TJppBasicPngButtonEx.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppBasicPngButtonEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppBasicPngButtonEx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
end;

{$IFDEF FPC}
function TJppBasicPngButtonEx.DrawTextBiDiModeFlags(Flags: Longint): Longint;
begin
  Result := DrawCtrlTextBiDiModeFlags(Self, Flags);
end;

procedure TJppBasicPngButtonEx.WndProc(var TheMessage: TLMessage);
var
  dis: TDrawItemStruct;
begin
  //inherited WndProc(TheMessage);
  case TheMessage.msg of
    LM_DRAWITEM:
      begin
        dis := PDrawItemStruct(TheMessage.lParam)^;
        DrawItem(dis);
      end;
    //LM_PAINT
  else
    inherited WndProc(TheMessage);
  end;

end;

procedure TJppBasicPngButtonEx.LMDrawItem(var Message: TLMDrawItems);
begin
  inherited;
  DrawItem(Message.DrawItemStruct^);
end;

procedure TJppBasicPngButtonEx.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

{$ENDIF}


  {$region ' --------------------------------- misc ------------------------------------ '}
procedure TJppBasicPngButtonEx.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      //Copy image from action's imagelist
      if (pngimage.Empty or FImageFromAction) and (ActionList <> nil) and (ActionList.Images <> nil) and (ImageIndex >= 0) and
        (ImageIndex < ActionList.Images.Count) then
      begin
        CopyImageFromImageList(FPngImage, ActionList.Images, ImageIndex);
        FImageFromAction := true;
      end;
    end;
  end;
end;


procedure TJppBasicPngButtonEx.SetAppearance(const Value: TJppBasicPngButtonExAppearance);
begin
  FAppearance := Value;
end;

{$IFDEF DCC}
procedure TJppBasicPngButtonEx.SetButtonStyle(ADefault: Boolean);
begin
  inherited SetButtonStyle(ADefault);
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;
{$ENDIF}

function TJppBasicPngButtonEx.PngImageStored: Boolean;
begin
  Result := not FImageFromAction;
end;

procedure TJppBasicPngButtonEx.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppBasicPngButtonEx.SetPngImage(const Value: TPngImage);
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

procedure TJppBasicPngButtonEx.SetPngOptions(const Value: TPngOptions);
begin
  if FPngOptions <> Value then
  begin
    FPngOptions := Value;
    Repaint;
  end;
end;

procedure TJppBasicPngButtonEx.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;


{$endregion misc}


{$region ' ------------------------ CalcButtonLayoutEx --------------------------- '}

{$IFDEF DEBUG}{$DEFINE DEBUG_CBLE}{$ENDIF}

procedure CalcButtonLayoutEx(const Canvas: TCanvas; const Caption, SubCaption: string; CaptionAlignment: TAlignment; CaptionFont, SubCaptionFont: TFont;
  const Png: TPngImage; const ClientRect: TRect; Layout: TButtonLayout; Margin, Spacing, CaptionsMargin: integer; var PngPos: TPoint;
  var CaptionRect, SubCaptionRect: TRect; BiDiFlags: LongInt);

type
  TFontParams = record
    Name: string;
    Size: integer;
    Style: TFontStyles;
  end;

  procedure AssignFont(const Font: TFont);
  begin
    Canvas.Font.Name := Font.Name;
    Canvas.Font.Size := Font.Size;
    Canvas.Font.Style := Font.Style;
  end;

var
  bCaption, bSubCaption, bPng: Boolean;
  OriginalFont: TFontParams;
  CaptionSize, SubCaptionSize, PngSize: TSize;
  dx: integer;
begin

  OriginalFont.Name := Canvas.Font.Name;
  OriginalFont.Size := Canvas.Font.Size;
  OriginalFont.Style := Canvas.Font.Style;

  bPng := (Assigned(Png)) and (not Png.Empty);
  if bPng then
  begin
    PngSize.Create(Png.Width, Png.Height);
    if (PngSize.Width <= 0) or (PngSize.Height <= 0) then bPng := False;
  end;

  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else if Layout = blGlyphRight then Layout := blGlyphLeft;

  bCaption := Caption <> '';
  bSubCaption := bCaption and (SubCaption <> '');

  CaptionSize.Create(0, 0);
  SubCaptionSize.Create(0, 0);

  if Margin < 0 then Margin := 0;

  try

    if bCaption then
    begin
      AssignFont(CaptionFont);
      CaptionSize := Canvas.TextExtent(Caption);
    end;

    if bSubCaption then
    begin
      AssignFont(SubCaptionFont);
      SubCaptionSize := Canvas.TextExtent(SubCaption);
    end;



    if bCaption then
    begin

      case CaptionAlignment of

        taCenter:
          begin
            CaptionRect.Left := (ClientRect.Width div 2) - (CaptionSize.Width div 2);
            CaptionRect.Width := CaptionSize.Width;
            CaptionRect.Top := (ClientRect.Height div 2) - (CaptionSize.Height div 2);
            CaptionRect.Height := CaptionSize.Height;
            if CaptionRect.Left < Margin then CaptionRect.Left := Margin;


            if bSubCaption then
            begin
              CaptionRect.Top := (ClientRect.Height div 2) - ( (CaptionSize.Height + CaptionsMargin + SubCaptionSize.Height) div 2 );
              CaptionRect.Height := CaptionSize.Height;

              SubCaptionRect.Top := CaptionRect.Bottom + CaptionsMargin;
              SubCaptionRect.Height := SubCaptionSize.Height;
              SubCaptionRect.Left := (ClientRect.Width div 2) - (SubCaptionSize.Width div 2);
              SubCaptionRect.Width := SubCaptionSize.Width;
              if SubCaptionRect.Left < Margin then SubCaptionRect.Left := Margin;
            end;
          end;

        taLeftJustify:
          begin
            CaptionRect.Left := Margin;
            CaptionRect.Width := CaptionSize.Width;
            CaptionRect.Top := (ClientRect.Height div 2) - (CaptionSize.Height div 2);
            CaptionRect.Height := CaptionSize.Height;

            if bSubCaption then
            begin
              CaptionRect.Top := (ClientRect.Height div 2) - ( (CaptionSize.Height + CaptionsMargin + SubCaptionSize.Height) div 2 );
              CaptionRect.Height := CaptionSize.Height;

              SubCaptionRect.Top := CaptionRect.Bottom + CaptionsMargin;
              SubCaptionRect.Height := SubCaptionSize.Height;
              SubCaptionRect.Left := Margin;
              SubCaptionRect.Width := SubCaptionSize.Width;
            end;
          end;

        taRightJustify:
          begin
            CaptionRect.Left := ClientRect.Width - CaptionSize.Width - Margin;
            CaptionRect.Width := CaptionSize.Width;
            CaptionRect.Top := (ClientRect.Height div 2) - (CaptionSize.Height div 2);
            CaptionRect.Height := CaptionSize.Height;
            //if CaptionRect.Left < Margin then CaptionRect.Left := Margin;
            if bSubCaption then
            begin
              CaptionRect.Top := (ClientRect.Height div 2) - ( (CaptionSize.Height + CaptionsMargin + SubCaptionSize.Height) div 2 );
              CaptionRect.Height := CaptionSize.Height;
              SubCaptionRect.Top := CaptionRect.Bottom + CaptionsMargin;
              SubCaptionRect.Height := SubCaptionSize.Height;
              SubCaptionRect.Left := ClientRect.Width - SubCaptionSize.Width - Margin;
              SubCaptionRect.Width := SubCaptionSize.Width;
            end;
          end;

      end; // case CaptionAlignment


      if (CaptionRect.Left + CaptionRect.Width) > ClientRect.Width then CaptionRect.Width := ClientRect.Width - CaptionRect.Left - 4;
      if bSubCaption then
        if (SubCaptionRect.Left + SubCaptionRect.Width) > ClientRect.Width then SubCaptionRect.Width := ClientRect.Width - SubCaptionRect.Left - 4;

    end; // if bCaption



    if bPng then
    begin

      case Layout of

        blGlyphLeft:
          begin
            PngPos.Y := (ClientRect.Height div 2) - (PngSize.Height div 2);

            case CaptionAlignment of

              taLeftJustify:
                begin
                  PngPos.X := Margin;
                  dx := PngSize.Width + Spacing;
                  OffsetRect(CaptionRect, dx, 0);
                  OffsetRect(SubCaptionRect, dx, 0);
                end;

              taCenter:
                begin
                  PngPos.X := (ClientRect.Width - CaptionSize.Width - PngSize.Width - Spacing) div 2;
                  if PngPos.X < Margin then PngPos.X := Margin;
                  CaptionRect.Left := PngPos.X + PngSize.Width + Spacing;
                  CaptionRect.Width := CaptionSize.Width;

                  if bSubCaption then
                  begin
                    if SubCaptionSize.Width > CaptionSize.Width then
                    begin
                      PngPos.X := (ClientRect.Width - SubCaptionSize.Width - PngSize.Width - Spacing) div 2;
                      if PngPos.X < Margin then PngPos.X := Margin;
                      SubCaptionRect.Left := PngPos.X + PngSize.Width + Spacing;
                    end
                    else
                      SubCaptionRect.Left := (ClientRect.Width div 2) - (SubCaptionSize.Width div 2) + (PngSize.Width div 2) + (Spacing div 2);

                    dx := PngPos.X + PngSize.Width + Spacing;
                    if SubCaptionRect.Left < dx then SubCaptionRect.Left := dx;
                    SubCaptionRect.Width := SubCaptionSize.Width;
                  end;
                end;

              taRightJustify:
                begin
                  PngPos.X := ClientRect.Width - CaptionSize.Width - PngSize.Width - Spacing - Margin;
                  if PngPos.X < Margin then PngPos.X := Margin;
                  dx := Margin + PngSize.Width + Spacing;
                  if CaptionRect.Left < dx then CaptionRect.Left := dx;;

                  if bSubCaption then
                  begin
                    if SubCaptionSize.Width > CaptionSize.Width then
                    begin
                      PngPos.X := ClientRect.Width - SubCaptionSize.Width - PngSize.Width - Spacing - Margin;
                      if PngPos.X < Margin then PngPos.X := Margin;
                      SubCaptionRect.Left := PngPos.X + PngSize.Width + Spacing;
                    end;
                    dx := Margin + PngSize.Width + Spacing;
                    if SubCaptionRect.Left < dx then SubCaptionRect.Left := dx;
                    SubCaptionRect.Width := SubCaptionSize.Width;
                  end;
                end;

            end; // case CaptionAlignment

          end; // blGlyphLeft


        blGlyphRight:
          begin
            PngPos.Y := (ClientRect.Height div 2) - (PngSize.Height div 2);

            case CaptionAlignment of
              taLeftJustify:
                begin
                  PngPos.X := CaptionRect.Right + Spacing;
                  if bSubCaption then
                    if SubCaptionSize.Width > CaptionSize.Width then PngPos.X := SubCaptionRect.Right + Spacing;
                end;

              taRightJustify:
                begin
                  OffsetRect(CaptionRect, -(Margin + Spacing + PngSize.Width), 0);
                  PngPos.X := CaptionRect.Right + Spacing;
                  if bSubCaption then OffsetRect(SubCaptionRect, -(Margin + Spacing + PngSize.Width), 0);
                end;

              taCenter:
                begin
                  PngPos.X := (ClientRect.Width div 2) + (CaptionSize.Width div 2) + (PngSize.Width div 2) + (Spacing div 2);
                  if bSubCaption then
                    if SubCaptionSize.Width > CaptionRect.Width then
                      PngPos.X := (ClientRect.Width div 2) + (SubCaptionSize.Width div 2) + (PngSize.Width div 2) + (Spacing div 2);
                end;

            end; // case CaptionAlignment

          end; // blGlyphRight


        blGlyphTop:
          begin

            PngPos.Y := (ClientRect.Height div 2) - (PngSize.Height + Spacing + CaptionSize.Height) div 2;
            CaptionRect.Top := PngPos.Y + PngSize.Height + Spacing;
            CaptionRect.Height := CaptionSize.Height;

            if bSubCaption then
            begin
              PngPos.Y := PngPos.Y - (CaptionsMargin + SubCaptionSize.Height) div 2;
              CaptionRect.Top := PngPos.Y + PngSize.Height + Spacing;
              CaptionRect.Height := CaptionSize.Height;
              SubCaptionRect.Top := CaptionRect.Bottom + CaptionsMargin;
              SubCaptionRect.Height := SubCaptionSize.Height;
            end;

            case CaptionAlignment of
              taCenter: PngPos.X := (ClientRect.Width div 2) - (PngSize.Width div 2);
              taLeftJustify: PngPos.X := Margin;
              taRightJustify: PngPos.X := ClientRect.Width - PngSize.Width - Margin;
            end; // case CaptionAlignment

          end;

        blGlyphBottom:
          begin
            PngPos.Y := (ClientRect.Height div 2) + (CaptionSize.Height div 2) - (PngSize.Height div 2) + (Spacing div 2);
            CaptionRect.Top := PngPos.Y - CaptionSize.Height - Spacing;
            CaptionRect.Height := CaptionSize.Height;

            if bSubCaption then
            begin
              PngPos.Y := PngPos.Y + (CaptionsMargin + SubCaptionSize.Height) div 2;
              CaptionRect.Top := PngPos.Y - CaptionSize.Height - SubCaptionSize.Height - Spacing - CaptionsMargin;
              CaptionRect.Height := CaptionSize.Height;
              SubCaptionRect.Top := CaptionRect.Bottom + CaptionsMargin;
              SubCaptionRect.Height := SubCaptionSize.Height;
            end;

            case CaptionAlignment of
              taCenter: PngPos.X := (ClientRect.Width div 2) - (PngSize.Width div 2);
              taLeftJustify: PngPos.X := Margin;
              taRightJustify: PngPos.X := ClientRect.Width - PngSize.Width - Margin;
            end;
          end;


      end; // case Layout


      if (CaptionRect.Left + CaptionRect.Width) > ClientRect.Width then CaptionRect.Width := ClientRect.Width - CaptionRect.Left - 4;
      if bSubCaption then
        if (SubCaptionRect.Left + SubCaptionRect.Width) > ClientRect.Width then SubCaptionRect.Width := ClientRect.Width - SubCaptionRect.Left - 4;

    end; // if bPng






  finally
    Canvas.Font.Name := OriginalFont.Name;
    Canvas.Font.Size := OriginalFont.Size;
    Canvas.Font.Style := OriginalFont.Style;

    {$IFDEF DEBUG}
      {$IFDEF DEBUG_CBLE}
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;

      Canvas.Pen.Color := clRed;
      Canvas.Rectangle(CaptionRect);

      Canvas.Pen.Color := clBlue;
      Canvas.Rectangle(SubCaptionRect);

      Canvas.Pen.Style := psDot;
      Canvas.Pen.Color := clGray;
      Canvas.Rectangle(TRect.Union(CaptionRect, SubCaptionRect).InflatedRect(1, 1));
      {$ENDIF}
    {$ENDIF}
  end;

end;
{$endregion CalcButtonLayoutEx}


  {$region ' -------------------------------------------------------- CNDrawItem ------------------------------------------------------------ '}

procedure TJppBasicPngButtonEx.CNDrawItem(var Message: {$IFDEF DCC}TWMDrawItem{$ELSE}TLMDrawItems{$ENDIF});
begin
  DrawItem(TDrawItemStruct(Message.DrawItemStruct^));
end;


procedure TJppBasicPngButtonEx.DrawItem(const DrawItemStruct: TDrawItemStruct);

type
  TGradientParams = record
    AngleDegree: Word;
    Balance: Word;
    BalanceMode: JPP.Gradient.TDgradBalanceMode;
    MaxDegrade: Byte;
    Orientation: JPP.Gradient.TDgradOrientation;
    SpeedPercent: integer;
    ColorFrom, ColorTo: TColor;
  end;

  TDrawingParams = record
    ColorGradientStart, ColorGradientEnd, Color: TColor;
    bGradientEnabled: Boolean;
    GradientType: TJppGradientType;
    GradientSteps: Byte;
    Pen: TPen;
    Font: TFont;
    GradientParams: TGradientParams;
    BorderToGradientMargin: integer;
    TransparentBackground: Boolean;
    TransparentFrame: Boolean;
    SubCaptionColor: TColor;
    SubCaptionFont: TFont;
  end;

  procedure CopyGradientParams(var GradientParams: TGradientParams; GradientEx: TJppGradientEx);
  begin
    if not Assigned(GradientEx) then Exit;
    GradientParams.AngleDegree := GradientEx.AngleDegree;
    GradientParams.Balance := GradientEx.Balance;
    GradientParams.BalanceMode := GradientEx.BalanceMode;
    GradientParams.MaxDegrade := GradientEx.MaxDegrade;
    GradientParams.Orientation := GradientEx.Orientation;
    GradientParams.SpeedPercent := GradientEx.SpeedPercent;
    GradientParams.ColorFrom := GradientEx.ColorFrom;
    GradientParams.ColorTo := GradientEx.ColorTo;
  end;

var
  PaintRect: TRect;
  GlyphPos: TPoint;
  Flags: Cardinal;
  {$IFDEF DCC}
  Button: TThemedButton;
  Details: TThemedElementDetails;
  {$ENDIF}

  bDown, bDefault, bSubCaption: Boolean;
  R, R2, BgRect: TRect;
  Canvas: TCanvas;
  imgDisabled, imgHot: TPngImage;
  dp: TDrawingParams;
  sCaption, sSubCaption: string;
  CaptionRect, SubCaptionRect: TRect;
  CaptionTextFlags, SubCaptionTextFlags: Cardinal;

begin

  SubCaptionTextFlags := DT_TOP or DT_LEFT or DT_SINGLELINE;
  case FAppearance.SubCaptionEllipsis of
    epPathEllipsis: SubCaptionTextFlags := SubCaptionTextFlags or DT_PATH_ELLIPSIS;
    epEndEllipsis: SubCaptionTextFlags := SubCaptionTextFlags or DT_END_ELLIPSIS;
    epWordEllipsis: SubCaptionTextFlags := SubCaptionTextFlags or DT_WORD_ELLIPSIS;
  end;

  CaptionTextFlags := DT_TOP or DT_LEFT or DT_SINGLELINE;
  case FAppearance.CaptionEllipsis of
    epPathEllipsis: CaptionTextFlags := CaptionTextFlags or DT_PATH_ELLIPSIS;
    epEndEllipsis: CaptionTextFlags := CaptionTextFlags or DT_END_ELLIPSIS;
    epWordEllipsis: CaptionTextFlags := CaptionTextFlags or DT_WORD_ELLIPSIS;
  end;

  bDown := DrawItemStruct.itemState and ODS_SELECTED <> 0;
  bDefault := DrawItemStruct.itemState and ODS_FOCUS <> 0;
  bSubCaption := (FAppearance.SubCaption <> '') and FAppearance.ShowCaption;

  dp.Font := Self.Font;
  dp.SubCaptionFont := FAppearance.SubCaptionFont;

  if not Appearance.DefaultDrawing then
  begin


    dp.TransparentBackground := true;


    Canvas := TCanvas.Create;
    try

      Canvas.Handle := DrawItemStruct.{$IFDEF FPC}_hDC{$ELSE}hDC{$ENDIF};
      R := ClientRect;

      with Canvas do
      begin


        dp.bGradientEnabled := True;
        dp.TransparentBackground := False;
        dp.TransparentFrame := False;
        dp.Pen := Pen;
        dp.Font := Font;


        {$region ' ----------- Disabled, Hot, Down, Focused, Normal ------------ '}

        // Disabled
        if not Enabled then
        begin
          dp.bGradientEnabled := Appearance.Disabled.GradientEnabled;
          if dp.bGradientEnabled then
          begin
            CopyGradientParams(dp.GradientParams, Appearance.Disabled.Gradient);
          end;
          dp.Color := Appearance.Disabled.Color;
          dp.Pen := Appearance.Disabled.Border;
          dp.Font := Appearance.Disabled.Font;
          dp.BorderToGradientMargin := Appearance.Disabled.BorderToGradientMargin;
          dp.TransparentBackground := Appearance.Disabled.TransparentBackground;
          dp.TransparentFrame := Appearance.Disabled.TransparentFrame;
          dp.SubCaptionColor := Appearance.Disabled.SubCaptionColor;
        end

        else

        begin

          // Hot
          if bOver and (not bDown) then
          begin
            dp.bGradientEnabled := Appearance.Hot.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.GradientParams, Appearance.Hot.Gradient);
            end;
            dp.Color := Appearance.Hot.Color;
            dp.Pen := Appearance.Hot.Border;
            dp.Font := Appearance.Hot.Font;
            dp.BorderToGradientMargin := Appearance.Hot.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Hot.TransparentBackground;
            dp.TransparentFrame := Appearance.Hot.TransparentFrame;
            dp.SubCaptionColor := Appearance.Hot.SubCaptionColor;
          end

          // Pressed
          else if bDown then
          begin
            dp.bGradientEnabled := Appearance.Down.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.GradientParams, Appearance.Down.Gradient);
            end;
            dp.Color := Appearance.Down.Color;
            dp.Pen := Appearance.Down.Border;
            dp.Font := Appearance.Down.Font;
            dp.BorderToGradientMargin := Appearance.Down.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Down.TransparentBackground;
            dp.TransparentFrame := Appearance.Down.TransparentFrame;
            dp.SubCaptionColor := Appearance.Down.SubCaptionColor;
          end

          // Focused
          else if Focused then
          begin
            dp.bGradientEnabled := Appearance.Focused.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.GradientParams, Appearance.Focused.Gradient);
            end;
            dp.Color := Appearance.Focused.Color;
            dp.Pen := Appearance.Focused.Border;
            dp.Font := Appearance.Focused.Font;
            dp.BorderToGradientMargin := Appearance.Focused.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Focused.TransparentBackground;
            dp.TransparentFrame := Appearance.Focused.TransparentFrame;
            dp.SubCaptionColor := Appearance.Focused.SubCaptionColor;
          end

          else

          // Normal
          begin
            dp.bGradientEnabled := Appearance.Normal.GradientEnabled;
            if dp.bGradientEnabled then
            begin
              CopyGradientParams(dp.GradientParams, Appearance.Normal.Gradient);
            end;
            dp.Color := Appearance.Normal.Color;
            dp.Pen := Appearance.Normal.Border;
            dp.Font := Appearance.Normal.Font;
            dp.BorderToGradientMargin := Appearance.Normal.BorderToGradientMargin;
            dp.TransparentBackground := Appearance.Normal.TransparentBackground;
            dp.TransparentFrame := Appearance.Normal.TransparentFrame;
            dp.SubCaptionColor := Appearance.Normal.SubCaptionColor;
          end;

        end;
        {$endregion Disabled, Hot, Down, Focused, Normal}


        {$region ' ------- Background --------- '}

        Brush.Style := bsSolid;
        Brush.Color := dp.Color;
        if not dp.TransparentBackground then FillRect(Canvas.ClipRect);

        if dp.bGradientEnabled and (not dp.TransparentBackground) then
        begin
          BgRect := R;

          if dp.BorderToGradientMargin < 0 then dp.BorderToGradientMargin := 0;

          BgRect.Bottom := BgRect.Bottom - dp.BorderToGradientMargin;
          BgRect.Left := BgRect.Left + dp.BorderToGradientMargin;
          BgRect.Right := BgRect.Right - dp.BorderToGradientMargin;
          BgRect.Top := BgRect.Top + dp.BorderToGradientMargin;

          Jpp.Gradient.cyGradientFill(
            Canvas,
            BgRect,
            dp.GradientParams.ColorFrom,
            dp.GradientParams.ColorTo,
            dp.GradientParams.Orientation,
            dp.GradientParams.Balance,
            dp.GradientParams.AngleDegree,
            dp.GradientParams.BalanceMode,
            dp.GradientParams.MaxDegrade,
            dp.GradientParams.SpeedPercent
          );
        end;
        {$endregion Background}


        {$region ' --------- Frame ----------- '}
        Brush.Style := bsClear;

        if not dp.TransparentFrame then
        begin
          if bDefault and Enabled and (not bDown) and (not Focused) then Pen.Assign(Appearance.BorderWhenDefault)
          else Pen.Assign(dp.Pen);
          //JppFrame3D(Canvas, R, Pen.Color, Pen.Width);
          DrawRectEx(Canvas, R, True, True, True, True, True);
        end;
        {$endregion Frame}


        {$region ' --------- Focus Rectangle ----------- '}
        if Focused and (not dp.TransparentFrame) then
        begin

          if Appearance.FocusRect.FocusType = frtNone then
          begin
            // do not draw the focus rectangle
          end
          else
          begin
            R2 := R;
            InflateRect(R2, -Appearance.FocusRect.Spacing, -Appearance.FocusRect.Spacing);

            if Appearance.FocusRect.FocusType = frtCustom then
            begin
              Brush.Style := bsClear;
              Pen.Assign(Appearance.FocusRect.Pen);
              JppFrame3D(Canvas, R2, Pen.Color, Pen.Width);
            end
            else if Appearance.FocusRect.FocusType = frtSystem then
            begin
              Brush.Style := bsSolid;
              Brush.Color := clBtnFace;
              DrawFocusRect(R2);
            end;
          end;

        end;
        {$endregion Focus rectangle}


        {$region ' -------- Calculating layout -------- '}
        sCaption := '';
        sSubCaption := '';
        if FAppearance.ShowCaption then
        begin
          sCaption := Caption;
          sSubCaption := FAppearance.SubCaption;
        end;
        CalcButtonLayoutEx(Canvas, sCaption, sSubCaption, FAppearance.CaptionAlignment, dp.Font, dp.SubCaptionFont, FPngImage, ClientRect,
          Layout, Margin, Spacing, FAppearance.CaptionsMargin, GlyphPos, CaptionRect, SubCaptionRect,
          DrawTextBiDiModeFlags(0));
        {$endregion Calculating layout}



        {$region ' ------------ Glyph (PNG) ------------ '}
        if bDown and Appearance.MoveWhenDown then
        begin
          Inc(GlyphPos.X);
          Inc(GlyphPos.Y);
        end;

        if (FPngImage <> nil) {and (Kind = bkCustom)} and not FPngImage.Empty then
        begin

          if Enabled and (bOver or bDown) then
          begin
            imgHot := TPngImage.Create;
            try
              imgHot.Assign(FPngImage);
              PngSetGamma(imgHot, Appearance.GlyphHotGammaFactor / 100);
              Draw(GlyphPos.X, GlyphPos.Y, imgHot)
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
              MakeImageGrayscale(imgDisabled, Appearance.GlyphDisabledGrayscaleFactor);
              MakeImageBlended(imgDisabled, Appearance.GlyphDisabledBlendFactor);
              Draw(GlyphPos.X, GlyphPos.Y, imgDisabled)
            finally
              imgDisabled.Free;
            end;
          end;

        end;
        {$endregion Glyph (PNG)}


        {$region ' ----------- Caption & SubCaption ------------- '}

        if Appearance.ShowCaption and (Length(Caption) > 0) then
        begin


          if bDown and Appearance.MoveWhenDown then
          begin
            OffsetRect(CaptionRect, 1, 1);
            if bSubCaption then OffsetRect(SubCaptionRect, 1, 1);
          end;

          Canvas.Brush.Style := bsClear;

          // Draw Caption
          Canvas.Font.Assign(dp.Font);
          DrawText(Canvas.Handle, PChar(Caption), -1, CaptionRect, Cardinal(DrawTextBiDiModeFlags(0)) or CaptionTextFlags);

          // Draw SubCaption
          if bSubCaption then
          begin
            Canvas.Font.Assign(dp.SubCaptionFont);
            Canvas.Font.Color := dp.SubCaptionColor;
            DrawText(Canvas.Handle, PChar(FAppearance.SubCaption), -1, SubCaptionRect, Cardinal(DrawTextBiDiModeFlags(0)) or SubCaptionTextFlags);
          end;

        end;

        {$endregion Caption & SubCaption}



      end; // with Canvas



    finally
      Canvas.Free;
    end;


  end



  else



  {$region ' ------------------- Deafult Drawing -------------------- '}

  begin

    R := ClientRect;
    FCanvas.Handle := DrawItemStruct.{$IFDEF FPC}_hDC{$ELSE}hDC{$ENDIF};
    FCanvas.Font := Self.Font;


    {$IFDEF DELPHIXE2_OR_ABOVE}
    //Draw the border
    if StyleServices.Enabled then
    begin
      //Themed border
      if not Enabled then Button := tbPushButtonDisabled
      else if bDown then Button := tbPushButtonPressed
      else if FMouseInControl then Button := tbPushButtonHot
      else if IsFocused or bDefault then Button := tbPushButtonDefaulted
      else Button := tbPushButtonNormal;

      //Paint the background, border, and finally get the inner rect
      Details := StyleServices.GetElementDetails(Button);
      StyleServices.DrawParentBackground(Handle, DrawItemStruct.HDC, @Details, true);
      StyleServices.DrawElement(DrawItemStruct.HDC, Details, DrawItemStruct.rcItem);
      StyleServices.GetElementContentRect(FCanvas.Handle, Details, DrawItemStruct.rcItem, R);
    end

    else
    {$ENDIF}

    begin

      //Draw the outer border, when focused
      if IsFocused or bDefault then
      begin
        FCanvas.Pen.Color := clWindowFrame;
        FCanvas.Pen.Width := 1;
        FCanvas.Brush.Style := bsClear;
        FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        InflateRect(R, -1, -1);
      end;

      //Draw the inner border
      if bDown then
      begin
        FCanvas.Pen.Color := clBtnShadow;
        FCanvas.Pen.Width := 1;
        FCanvas.Brush.Color := clBtnFace;
        FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        InflateRect(R, -1, -1);
      end

      else

      begin
        Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
        if DrawItemStruct.itemState and ODS_DISABLED <> 0 then Flags := Flags or DFCS_INACTIVE;
        DrawFrameControl(DrawItemStruct.{$IFDEF FPC}_hDC{$ELSE}hDC{$ENDIF}, R, DFC_BUTTON, Flags);
      end;


      //Adjust the rect when focused and/or down
//      if IsFocused then
//      begin
//        R := ClientRect;
//        InflateRect(R, -1, -1);
//      end;
//
//      if bDown then OffsetRect(R, 1, 1);
    end;



    {$region ' -------- Calculating layout -------- '}

    sCaption := '';
    sSubCaption := '';
    if FAppearance.ShowCaption then
    begin
      sCaption := Caption;
      sSubCaption := FAppearance.SubCaption;
    end;

    CalcButtonLayoutEx(FCanvas, sCaption, sSubCaption, FAppearance.CaptionAlignment, Self.Font, FAppearance.SubCaptionFont,
      FPngImage, ClientRect, Layout, Margin, Spacing, FAppearance.CaptionsMargin, GlyphPos, CaptionRect, SubCaptionRect, DrawTextBiDiModeFlags(0));
    {$endregion Calculating layout}



    if bDown and FAppearance.MoveWhenDown then
    begin
      Inc(GlyphPos.X);
      Inc(GlyphPos.Y);
    end;

    //Draw the image
    if (FPngImage <> nil) and (Kind = bkCustom) and not FPngImage.Empty then
    begin
      PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, FPngImage.Width, FPngImage.Height);
      if Enabled then DrawPNG(FPngImage, FCanvas, PaintRect, [])
      else DrawPNG(FPngImage, FCanvas, PaintRect, FPngOptions);
    end;



    //Draw the text
    if Appearance.ShowCaption and (Length(Caption) > 0) then
    begin

      if bDown and Appearance.MoveWhenDown then
      begin
        OffsetRect(CaptionRect, 1, 1);
        if bSubCaption then OffsetRect(SubCaptionRect, 1, 1);
      end;

      FCanvas.Brush.Style := bsClear;

      //grayed (shadowed) Caption when disabled
      if not Enabled then
      begin

        // Draw Caption
        OffsetRect(CaptionRect, 1, 1);
        FCanvas.Font.Assign(Self.Font);
        FCanvas.Font.Color := clBtnHighlight;
        DrawText(FCanvas.Handle, PChar(Caption), -1, CaptionRect, DrawTextBiDiModeFlags(0) or DT_TOP or DT_LEFT or DT_SINGLELINE);
        OffsetRect(CaptionRect, -1, -1);

        // Draw SubCaption
        if bSubCaption then
        begin
          OffsetRect(SubCaptionRect, 1, 1);
          FCanvas.Font.Assign(dp.SubCaptionFont);
          FCanvas.Font.Color := clBtnHighlight;
          DrawText(FCanvas.Handle, PChar(FAppearance.SubCaption), -1, SubCaptionRect, Cardinal(DrawTextBiDiModeFlags(0)) or SubCaptionTextFlags);
          OffsetRect(SubCaptionRect, -1, -1);
          FCanvas.Font.Assign(Self.Font);
        end;

        FCanvas.Font.Color := clBtnShadow;
      end;


      // Draw Caption
      DrawText(FCanvas.Handle, PChar(Caption), -1, CaptionRect, DrawTextBiDiModeFlags(0) or DT_TOP or DT_LEFT or DT_SINGLELINE);

      // Draw SubCaption
      if bSubCaption then
      begin
        FCanvas.Font.Assign(FAppearance.SubCaptionFont);
        if not Enabled then FCanvas.Font.Color := clBtnShadow;
        DrawText(FCanvas.Handle, PChar(FAppearance.SubCaption), -1, SubCaptionRect, Cardinal(DrawTextBiDiModeFlags(0)) or SubCaptionTextFlags);
      end;

    end;



    //Draw the focus rectangle
    if IsFocused and bDefault then
    begin
      {$IFDEF DELPHIXE2_OR_ABOVE}if not StyleServices.Enabled then{$ENDIF}
      begin
        R := ClientRect;
        InflateRect(R, -3, -3);
      end;
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Color := clBtnFace;
      DrawFocusRect(FCanvas.Handle, R);
    end;

    FLastKind := Kind;
    FCanvas.Handle := 0;

  end;
  {$endregion Default Drawing}


end;

  {$endregion CNDrawItem}


  {$region ' ---------------------- Mouse Enter & Leave --------------------------- '}
procedure TJppBasicPngButtonEx.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  FOnMouseEnter := Value;
end;

procedure TJppBasicPngButtonEx.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  FOnMouseLeave := Value;
end;

procedure TJppBasicPngButtonEx.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  bOver := True;
  {$IFDEF DELPHIXE2_OR_ABOVE}
  if StyleServices.Enabled and not FMouseInControl and not(csDesigning in ComponentState) then
  begin
    FMouseInControl := true;
    Repaint;
  end
  else
  {$ENDIF}
  begin
    if csDesigning in ComponentState then Exit;
    if Assigned(FOnMouseEnter) then OnMouseEnter(Self);
    Repaint;
  end;
end;

procedure TJppBasicPngButtonEx.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  bOver := False;
  {$IFDEF DELPHIXE2_OR_ABOVE}
  if StyleServices.Enabled and FMouseInControl then
  begin
    FMouseInControl := False;
    Repaint;
  end
  else
  {$ENDIF}
  begin
    if csDesigning in ComponentState then Exit;
    if Assigned(FOnMouseLeave) then OnMouseLeave(Self);
    Repaint;
  end;
end;
  {$endregion Mouse Enter & Leave}


{$endregion TJppBasicPngButtonEx}


{$region ' --------------------- TJppBasicPngButtonExAppearance -------------------- '}
constructor TJppBasicPngButtonExAppearance.Create(AOwner: TComponent);
begin
  inherited Create;

  FNormal := TJppBasicPngButtonExStateParams.Create(AOwner);
  FNormal.Color := $00F3F3F3;
  FNormal.Gradient.ColorFrom := $00F1F1F1;
  FNormal.Gradient.ColorTo := $00CFCFCF;
  FNormal.Border.Color := $00707070;
  FNormal.Font.Color := 0;

  FHot := TJppBasicPngButtonExStateParams.Create(AOwner);
  FHot.Color := $00FCF5E8;
  FHot.Gradient.ColorFrom := $00FDF6EA;
  FHot.Gradient.ColorTo := $00F5D9A7;
  FHot.Border.Color := $00B17F3C;
  FHot.Font.Color := 0;

  FFocused := TJppBasicPngButtonExStateParams.Create(AOwner);
  FFocused.Color := FNormal.Color;
  FFocused.Gradient.ColorFrom := FNormal.Gradient.ColorFrom;
  FFocused.Gradient.ColorTo := FNormal.Gradient.ColorTo;
  FFocused.Border.Color := $00D0AA24;
  FFocused.Font.Color := FNormal.Font.Color;

  FDown := TJppBasicPngButtonExStateParams.Create(AOwner);
  FDown.Color := $00F1E2C5;
  FDown.Gradient.ColorFrom := $00FCF4E5;
  FDown.Gradient.ColorTo := $00DFB972;
  FDown.Border.Color := GetSimilarColor(FHot.Border.Color , 50, False);
  FDown.Font.Color := 0;

  FDisabled := TJppBasicPngButtonExStateParams.Create(AOwner);
  FDisabled.Color := $00F4F4F4;
  FDisabled.Gradient.ColorFrom := $00F4F4F4;
  FDisabled.Gradient.ColorTo := $00F4F4F4;
  FDisabled.Border.Color := $00B5B2AD;
  FDisabled.Font.Color := RGB(160,160,160);
  FDisabled.SubCaptionColor := FDisabled.Font.Color;

  FFocusRect := TJppFocusRectParams.Create(AOwner);
  FFocusRect.Pen.Color := $00D0AA24;

  FBorderWhenDefault := TPen.Create;
  FBorderWhenDefault.Color := $00D0AA24;

  FGlyphDisabledGrayscaleFactor := 255;
  FGlyphDisabledBlendFactor := 127;
  FGlyphHotGammaFactor := 130;


  FNormal.OnChange := PropsChanged;
  FHot.OnChange := PropsChanged;
  FFocused.OnChange := PropsChanged;
  FDown.OnChange := PropsChanged;
  FDisabled.OnChange := PropsChanged;
  FFocusRect.OnChange := PropsChanged;

  FShowCaption := True;

  FSubCaptionFont := TFont.Create;
  FSubCaptionFont.OnChange := PropsChanged;
  FSubCaption := '';
  FCaptionsMargin := 2;
  FSubCaptionEllipsis := epEndEllipsis;
  FCaptionAlignment := taCenter;
  FCaptionEllipsis := epNone;

end;

destructor TJppBasicPngButtonExAppearance.Destroy;
begin
  FNormal.Free;
  FHot.Free;
  FFocused.Free;
  FDefaultDrawing := False;
  FDown.Free;
  FDisabled.Free;
  FFocusRect.Free;
  FBorderWhenDefault.Free;
  FSubCaptionFont.Free;
  inherited;
end;

procedure TJppBasicPngButtonExAppearance.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicPngButtonExAppearance.Assign(Source: TJppBasicPngButtonExAppearance);
begin

  DefaultDrawing := Source.DefaultDrawing;
  GlyphDisabledGrayscaleFactor := Source.GlyphDisabledGrayscaleFactor;
  GlyphDisabledBlendFactor := Source.GlyphDisabledBlendFactor;
  GlyphHotGammaFactor := Source.GlyphHotGammaFactor;
  MoveWhenDown := Source.MoveWhenDown;
  BorderWhenDefault.Assign(Source.BorderWhenDefault);

  FocusRect.FocusType := Source.FocusRect.FocusType;
  FocusRect.Spacing := Source.FocusRect.Spacing;
  FocusRect.Pen.Assign(Source.FocusRect.Pen);

  ShowCaption := Source.ShowCaption;
  SubCaptionFont.Assign(Source.SubCaptionFont);
  SubCaptionEllipsis := Source.SubCaptionEllipsis;
  CaptionAlignment := Source.CaptionAlignment;


  Normal.Font.Assign(Source.Normal.Font);
  Normal.Border.Assign(Source.Normal.Border);
  Normal.Color := Source.Normal.Color;
  Normal.GradientEnabled := Source.Normal.GradientEnabled;
  Normal.Gradient.Assign(Source.Normal.Gradient);
  Normal.BorderToGradientMargin := Source.Normal.BorderToGradientMargin;
  Normal.TransparentBackground := Source.Normal.TransparentBackground;
  Normal.TransparentFrame := Source.Normal.TransparentFrame;
  Normal.SubCaptionColor := Source.Normal.SubCaptionColor;

  Hot.Font.Assign(Source.Hot.Font);
  Hot.Border.Assign(Source.Hot.Border);
  Hot.Color := Source.Hot.Color;
  Hot.GradientEnabled := Source.Hot.GradientEnabled;
  Hot.Gradient.Assign(Source.Hot.Gradient);
  Hot.BorderToGradientMargin := Source.Hot.BorderToGradientMargin;
  Hot.TransparentBackground := Source.Hot.TransparentBackground;
  Hot.TransparentFrame := Source.Hot.TransparentFrame;
  Hot.SubCaptionColor := Source.Hot.SubCaptionColor;

  Down.Font.Assign(Source.Down.Font);
  Down.Border.Assign(Source.Down.Border);
  Down.Color := Source.Down.Color;
  Down.GradientEnabled := Source.Down.GradientEnabled;
  Down.Gradient.Assign(Source.Down.Gradient);
  Down.BorderToGradientMargin := Source.Down.BorderToGradientMargin;
  Down.TransparentBackground := Source.Down.TransparentBackground;
  Down.TransparentFrame := Source.Down.TransparentFrame;
  Down.SubCaptionColor := Source.Down.SubCaptionColor;

  Focused.Font.Assign(Source.Focused.Font);
  Focused.Border.Assign(Source.Focused.Border);
  Focused.Color := Source.Focused.Color;
  Focused.GradientEnabled := Source.Focused.GradientEnabled;
  Focused.Gradient.Assign(Source.Focused.Gradient);
  Focused.BorderToGradientMargin := Source.Focused.BorderToGradientMargin;
  Focused.TransparentBackground := Source.Focused.TransparentBackground;
  Focused.TransparentFrame := Source.Focused.TransparentFrame;
  Focused.SubCaptionColor := Source.Focused.SubCaptionColor;

  Disabled.Font.Assign(Source.Disabled.Font);
  Disabled.Border.Assign(Source.Disabled.Border);
  Disabled.Color := Source.Disabled.Color;
  Disabled.GradientEnabled := Source.Disabled.GradientEnabled;
  Disabled.Gradient.Assign(Source.Disabled.Gradient);
  Disabled.BorderToGradientMargin := Source.Disabled.BorderToGradientMargin;
  Disabled.TransparentBackground := Source.Disabled.TransparentBackground;
  Disabled.TransparentFrame := Source.Disabled.TransparentFrame;
  Disabled.SubCaptionColor := Source.Disabled.SubCaptionColor;

end;

procedure TJppBasicPngButtonExAppearance.SetBorderWhenDefault(const Value: TPen);
begin
  FBorderWhenDefault := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetCaptionAlignment(const Value: TAlignment);
begin
  if FCaptionAlignment = Value then Exit;
  FCaptionAlignment := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetCaptionEllipsis(const Value: TEllipsisPosition);
begin
  if FCaptionEllipsis = Value then Exit;
  FCaptionEllipsis := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetCaptionsMargin(const Value: integer);
begin
  if FCaptionsMargin = Value then Exit;
  FCaptionsMargin := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetDefaultDrawing(const Value: Boolean);
begin
  FDefaultDrawing := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetDisabled(const Value: TJppBasicPngButtonExStateParams);
begin
  FDisabled := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetGlyphDisabledBlendFactor(const Value: Byte);
begin
  FGlyphDisabledBlendFactor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetGlyphDisabledGrayscaleFactor(const Value: Byte);
begin
  FGlyphDisabledGrayscaleFactor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetDown(const Value: TJppBasicPngButtonExStateParams);
begin
  FDown := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetFocusRect(const Value: TJppFocusRectParams);
begin
  FFocusRect := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetFocused(const Value: TJppBasicPngButtonExStateParams);
begin
  FFocused := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetHot(const Value: TJppBasicPngButtonExStateParams);
begin
  FHot := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetGlyphHotGammaFactor(const Value: Byte);
begin
  FGlyphHotGammaFactor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetMoveWhenDown(const Value: Boolean);
begin
  FMoveWhenDown := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetNormal(const Value: TJppBasicPngButtonExStateParams);
begin
  FNormal := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetSubCaption(const Value: string);
begin
  FSubCaption := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetSubCaptionEllipsis(const Value: TEllipsisPosition);
begin
  if FSubCaptionEllipsis = Value then Exit;
  FSubCaptionEllipsis := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExAppearance.SetSubCaptionFont(const Value: TFont);
begin
  FSubCaptionFont.Assign(Value);
end;

{$endregion TJppBasicPngButtonExAppearance}


{$region ' -------------------- TJppBasicPngButtonExStateParams ---------------------- '}
constructor TJppBasicPngButtonExStateParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FGradient := TJppGradientEx.Create(AOwner);
  FGradientEnabled := True;
  FBorderToGradientMargin := 2;
  FFont := TFont.Create;
  FBorder := TPen.Create;
  FTransparentBackground := False;
  FTransparentFrame := False;

  FGradient.OnChange := PropsChanged;
  FFont.OnChange := PropsChanged;
  FBorder.OnChange := PropsChanged;

  FSubCaptionColor := clWindowText;
end;

destructor TJppBasicPngButtonExStateParams.Destroy;
begin
  FGradient.Free;
  FFont.Free;
  FBorder.Free;
  inherited;
end;

procedure TJppBasicPngButtonExStateParams.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetBorder(const Value: TPen);
begin
  FBorder := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetBorderToGradientMargin(const Value: integer);
begin
  FBorderToGradientMargin := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetColor(const Value: TColor);
begin
  FColor := Value;
  PropsChanged(Self);
end;


procedure TJppBasicPngButtonExStateParams.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetGradientEnabled(const Value: Boolean);
begin
  FGradientEnabled := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppBasicPngButtonExStateParams.SetSubCaptionColor(const Value: TColor);
begin
  if FSubCaptionColor = Value then Exit;
  FSubCaptionColor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetTransparentBackground(const Value: Boolean);
begin
  FTransparentBackground := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetTransparentFrame(const Value: Boolean);
begin
  FTransparentFrame := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPngButtonExStateParams.SetGradient(const Value: TJppGradientEx);
begin
  FGradient := Value;
  PropsChanged(Self);
end;


{$endregion TJppBasicPngButtonExStateParams}


{$region ' --------------------- Themes ------------------------- '}
{$IFDEF DCC}
{$IF RTLVersion < 23.0 }
type
  TThemeServicesHelper = class helper for TThemeServices
  private
    function GetEnabled: Boolean;
  public
    function GetElementContentRect(DC: HDC; Details: TThemedElementDetails; const BoundingRect: TRect; out ContentRect: TRect): Boolean; overload;
    property Enabled: Boolean read GetEnabled;
  end;

function TThemeServicesHelper.GetElementContentRect(DC: HDC; Details: TThemedElementDetails; const BoundingRect: TRect; out ContentRect: TRect): Boolean;
begin
  ContentRect := Self.ContentRect(DC, Details, BoundingRect);
  Result := true;
end;

function TThemeServicesHelper.GetEnabled: Boolean;
begin
  Result := ThemesEnabled;
end;

function StyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;
{$IFEND}
{$ENDIF} // DCC



{$IFDEF DELPHIXE3_OR_ABOVE}
class constructor TJppBasicPngButtonEx.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TJppBasicPngButtonEx, TJppBasicPngButtonExStyleHook);
end;

class destructor TJppBasicPngButtonEx.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TJppBasicPngButtonEx, TJppBasicPngButtonExStyleHook);
end;
{$ENDIF}
{$endregion Themes}


{$region ' ------------------------- StyleHook -------------------------- '}
{$IFDEF DCC}
{$IF RTLVersion >= 24.0 }
procedure TJppBasicPngButtonExStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
const
  WordBreakFlag: array [Boolean] of Integer = (0, DT_WORDBREAK);
var
  Details: TThemedElementDetails;
  DrawRect, PaintRect, TextRect: TRect;
  State: TButtonState;
  btn: TJppBasicPngButtonEx;
  GlyphPos, TextPos: TPoint;

  LColor: TColor;
  LFormats: TTextFormat;
begin
  if not(Control is TJppBasicPngButtonEx) then
  begin
    inherited;
    Exit;
  end;
  if FPressed then Details := StyleServices.GetElementDetails(tbPushButtonPressed)
  else if AMouseInControl then Details := StyleServices.GetElementDetails(tbPushButtonHot)
  else if Focused or TJppBasicPngButtonEx(Control).Default then Details := StyleServices.GetElementDetails(tbPushButtonDefaulted)
  else if Control.Enabled then Details := StyleServices.GetElementDetails(tbPushButtonNormal)
  else Details := StyleServices.GetElementDetails(tbPushButtonDisabled);
  DrawRect := Control.ClientRect;
  StyleServices.DrawElement(ACanvas.Handle, Details, DrawRect);

  btn := Control as TJppBasicPngButtonEx;
  ACanvas.Font := btn.Font;
  if not btn.Enabled then State := bsDisabled
  else if FPressed then State := bsDown
  else State := bsUp;

  //Calculate the position of the PNG glyph
  CalcButtonLayout(ACanvas, btn.FPngImage, btn.ClientRect, FPressed, False, btn.Caption, btn.Layout, btn.Margin, btn.Spacing, GlyphPos, TextPos,
    btn.DrawTextBiDiModeFlags(0));

  //Draw the image
  if (btn.FPngImage <> nil) and (btn.Kind = bkCustom) and not btn.FPngImage.Empty then
  begin
    PaintRect := Bounds(GlyphPos.X, GlyphPos.Y, btn.FPngImage.Width, btn.FPngImage.Height);
    if btn.Enabled then DrawPNG(btn.FPngImage, ACanvas, PaintRect, [])
    else DrawPNG(btn.FPngImage, ACanvas, PaintRect, btn.FPngOptions);
  end;

  ACanvas.Brush.Style := bsClear;
  if (State = bsDisabled) or (not StyleServices.IsSystemStyle and (seFont in btn.StyleElements)) then
  begin
    if not StyleServices.GetElementColor(Details, ecTextColor, LColor) or (LColor = clNone) then LColor := ACanvas.Font.Color;
  end
  else LColor := ACanvas.Font.Color;

  LFormats := TTextFormatFlags(DT_NOCLIP or DT_CENTER or DT_VCENTER or btn.DrawTextBiDiModeFlags(0) or WordBreakFlag[btn.WordWrap]);

  if Length(btn.Caption) > 0 then
  begin
    TextRect := Rect(0, 0, btn.ClientRect.Right - btn.ClientRect.Left, 0);
    DrawText(ACanvas.Handle, PChar(btn.Caption), Length(btn.Caption), TextRect, DT_CALCRECT or btn.DrawTextBiDiModeFlags(0));
  end
  else
  begin
    TextRect := Rect(0, 0, 0, 0);
  end;

  OffsetRect(TextRect, TextPos.X + btn.ClientRect.Left, TextPos.Y + btn.ClientRect.Top);
  StyleServices.DrawText(ACanvas.Handle, Details, btn.Caption, TextRect, LFormats, LColor);

end;
{$IFEND}
{$ENDIF} // DCC
{$endregion StyleHook}



{$ENDIF} // MSWINDOWS

end.
