unit FMX.StatusBar;

interface

uses
  System.UITypes, System.SysUtils, FMX.Dialogs, FMX.Platform, FMX.Forms, FMX.Graphics
{$IF defined(ANDROID)}
    , Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.OS, Androidapi.JNI.JavaTypes
{$ELSEIF defined(IOS)}
    , FMX.Helpers.iOS, FMX.Platform.iOS, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.CoreImage
{$ENDIF};

type
  TmyWindow = record
    class function Init: boolean; static;
    class var StatusBarHeight: Single;

    class procedure StatusBarColor(const aForm: TForm; const aColor: TAlphaColor); static;
  end;

implementation

{ TmyStatusBar }

class function TmyWindow.Init: boolean;
{$IFDEF ANDROID}
var
  resourceID: Integer;
  ScreenService: IFMXScreenService;
  sScale: Single;
  sAbis: string;
  arrAbis: TJavaObjectArray<JString>;
  I: Integer;
  needCheckStatusBarHeight: boolean;
{$ENDIF}
begin
  TmyWindow.StatusBarHeight := 0;
{$IFDEF ANDROID}
  if TOSVersion.Major >= 5 then
  begin
    sAbis := '';
    arrAbis := TJBuild.JavaClass.SUPPORTED_ABIS;
    for I := 0 to arrAbis.Length - 1 do
      sAbis := sAbis + ',' + JStringToString(arrAbis.Items[I]);
    sAbis := sAbis.trim([',']);
  end
  else
    sAbis := JStringToString(TJBuild.JavaClass.CPU_ABI) + ',' + JStringToString(TJBuild.JavaClass.CPU_ABI2);

  needCheckStatusBarHeight := (sAbis.Contains('x86') or JStringToString(TJBuild.JavaClass.FINGERPRINT).Contains('intel')
    or sAbis.Contains('arm64-v8a')) and (TOSVersion.Major >= 4);

  if (TOSVersion.Major >= 5) or (needCheckStatusBarHeight) then
  begin
    sScale := 1;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenService)) then
      sScale := ScreenService.GetScreenScale;

    resourceID := TAndroidHelper.Activity.getResources.getIdentifier(StringToJString('status_bar_height'),
      StringToJString('dimen'), StringToJString('android'));
    if resourceID > 0 then
      TmyWindow.StatusBarHeight := Trunc(TAndroidHelper.Activity.getResources.getDimensionPixelSize(resourceID)
        / sScale);
  end;
{$ENDIF}
end;

{$IF CompilerVersion < 32.0}
{$IFDEF IOS}

function GetStatusBarView: UIView;
var
  I: Integer;
  LViews: NSArray;
  LView: UIView;
begin
  Result := nil;
  LViews := SharedApplication.keyWindow.rootViewController.view.subviews;
  for I := 0 to LViews.count - 1 do
  begin
    LView := TUIView.Wrap(LViews.objectAtIndex(I));
    if CGRectEqualToRect(LView.frame, SharedApplication.statusBarFrame) <> 0 then
    begin
      Result := LView;
      Break;
    end;
  end;
end;

procedure SetStatusBarBackgroundColor(const ABackgroundColor: TAlphaColor);
var
  Red: Single;
  Green: Single;
  Blue: Single;
  ColorCI: CIColor;
  ColorUI: UIColor;
  StatusBarView: UIView;
begin
  StatusBarView := GetStatusBarView;
  if StatusBarView = nil then
    Exit;
  Red := TAlphaColorRec(ABackgroundColor).r / 255;
  Green := TAlphaColorRec(ABackgroundColor).g / 255;
  Blue := TAlphaColorRec(ABackgroundColor).b / 255;
  ColorCI := TCIColor.Wrap(TCIColor.OCClass.colorWithRed(Red, Green, Blue));
  ColorUI := TUIColor.Wrap(TUIColor.OCClass.colorWithCIColor(ColorCI));
  StatusBarView.setBackgroundColor(ColorUI);
  if TOSVersion.Check(7, 0) then
    SharedApplication.keyWindow.rootViewController.setNeedsStatusBarAppearanceUpdate;
end;
{$ENDIF}
{$ENDIF}

class procedure TmyWindow.StatusBarColor(const aForm: TForm; const aColor: TAlphaColor);
begin
  // Tokyo
{$IF CompilerVersion >= 32.0}
{$IFDEF IOS}
  aForm.SystemStatusBar.Visibility := TFormSystemStatusBar.TVisibilityMode.Visible;
  aForm.SystemStatusBar.BackgroundColor := aColor;
{$ELSE}
  if aForm.Fill.Kind <> TBrushKind.Solid then
    aForm.Fill.Kind := TBrushKind.Solid;
  aForm.Fill.Color := aColor;
{$ENDIF}
{$ENDIF}
  // Seattle, Berlin
{$IF CompilerVersion < 32.0}
{$IFDEF IOS}
  SetStatusBarBackgroundColor(aColor);
{$ELSE}
  if aForm.Fill.Kind <> TBrushKind.Solid then
    aForm.Fill.Kind := TBrushKind.Solid;
  aForm.Fill.Color := aColor;
{$ENDIF}
{$ENDIF}
end;

end.
