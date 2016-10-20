unit FMX.StatusBar;

{
  author : ZuBy
  https://github.com/rzaripov1990
  rzaripov1990@gmail.com

  2016
}

interface

uses
  System.UITypes, FMX.Dialogs, FMX.Platform
{$IFDEF ANDROID} ,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.OS,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Util, FMX.Platform.Android, FMX.Helpers.Android
{$ELSEIF defined(IOS)}
    , FMX.Helpers.iOS, FMX.Platform.iOS, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.CoreImage
{$ENDIF};

{$IFDEF ANDROID}

type
  JWindowManager_LayoutParamsClassExt = interface(JWindowManager_LayoutParamsClass)
    ['{48861EC2-BC53-440A-B1B8-76FF4DF36F97}']
    { class } function _GetFLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS: Integer; cdecl;
    { class } property FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS: Integer read _GetFLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS;
  end;

  TJWindowManager_LayoutParamsExt = class(TJavaGenericImport<JWindowManager_LayoutParamsClassExt,
    JWindowManager_LayoutParams>)
  end;

  [JavaSignature('android/view/Window')]
  JWindowExt = interface(JWindow)
    ['{2810B178-9634-453D-8963-19CE145E6669}']
    procedure setStatusBarColor(color: Integer); cdecl;

    function getStatusBarColor: Integer; cdecl;
  end;

  TJWindowExt = class(TJavaGenericImport<JWindowClass, JWindowExt>)
  end;

function GetWindowExt: JWindowExt;
{$ENDIF}
function Darker(color: TAlphaColor; Percent: Byte): TAlphaColor;

procedure StatusBarGetBounds(out StatusBar, NavigationBar: Integer);
procedure StatusBarSetColor(const aColor: TAlphaColor);
function NavBarOrientationBottom: boolean;

implementation

uses
  System.SysUtils;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;

  function MathRound(AValue: Extended): Int64; inline;
  begin
    if AValue >= 0 then
      Result := Trunc(AValue + 0.5)
    else
      Result := Trunc(AValue - 0.5);
  end;

begin
  if nDenominator = 0 then
    Result := -1
  else
    Result := MathRound(Int64(nNumber) * Int64(nNumerator) / nDenominator);
end;

function Darker(color: TAlphaColor; Percent: Byte): TAlphaColor;
var
  r, g, b: Byte;
begin
  r := TAlphaColorRec(color).r;
  g := TAlphaColorRec(color).g;
  b := TAlphaColorRec(color).b;
  r := r - MulDiv(r, Percent, 100);
  g := g - MulDiv(g, Percent, 100);
  b := b - MulDiv(b, Percent, 100);
  Result := TAlphaColorF.Create(r / 255, g / 255, b / 255, 1).ToAlphaColor;
end;

function Lighter(color: TAlphaColor; Percent: Byte): TAlphaColor;
var
  r, g, b: Byte;
begin
  r := TAlphaColorRec(color).r;
  g := TAlphaColorRec(color).g;
  b := TAlphaColorRec(color).b;
  r := r + MulDiv(255 - r, Percent, 100);
  g := g + MulDiv(255 - g, Percent, 100);
  b := b + MulDiv(255 - b, Percent, 100);
  Result := TAlphaColorF.Create(r / 255, g / 255, b / 255, 1).ToAlphaColor;
end;

{$IFDEF ANDROID}

function GetWindowExt: JWindowExt;
begin
  Result := TJWindowExt.Wrap((MainActivity.getWindow as ILocalObject).GetObjectID);
end;
{$ENDIF}

function NavBarOrientationBottom: boolean;
{$IFDEF ANDROID}
var
  visibleFrame: JRect;
  Window: JWindowExt;
  metrics: JDisplayMetrics;
{$ENDIF}
begin
  Result := false;
{$IFDEF ANDROID}
  visibleFrame := TJRect.Create;
  Window := GetWindowExt;
  Window.getDecorView.getWindowVisibleDisplayFrame(visibleFrame);

  metrics := TJDisplayMetrics.Create;
  Window.getWindowManager.getDefaultDisplay.getRealMetrics(metrics);
  Result := metrics.widthPixels = visibleFrame.right;

{$ENDIF}
end;

function hasNavbar(out navBarHeight: Integer): boolean;
// uses Androidapi.JNI.Util,
{$IFDEF ANDROID}
var
  Window: JWindowExt;
  realSize: JPoint;
  screenSize: JPoint;
  metrics: JDisplayMetrics;
  difference, resourceID: Integer;
{$ENDIF}
begin
  Result := false;
  navBarHeight := 0;
{$IFDEF ANDROID}
  metrics := TJDisplayMetrics.Create;
  realSize := TJPoint.Create;
  screenSize := TJPoint.Create;

  Window := GetWindowExt;
  Window.getWindowManager.getDefaultDisplay.getRealMetrics(metrics);
  realSize.x := metrics.widthPixels;
  realSize.y := metrics.heightPixels;

  Window.getWindowManager.getDefaultDisplay.getSize(screenSize);
  if realSize.y <> screenSize.y then
  begin
    difference := realSize.y - screenSize.y;
    resourceID := TAndroidHelper.Activity.getResources.getIdentifier(StringToJString('navigation_bar_height'),
      StringToJString('dimen'), StringToJString('android'));
    if resourceID > 0 then
      navBarHeight := Trunc(TAndroidHelper.Activity.getResources.getDimensionPixelSize(resourceID));

    if navBarHeight > 0 then
      Result := difference = navBarHeight;
  end;
{$ENDIF}
end;

procedure StatusBarGetBounds(out StatusBar, NavigationBar: Integer);
{$IFDEF ANDROID}
var
  resourceID: Integer;
  ScreenService: IFMXScreenService;
  sScale: Single;
  sAbis: string;
  arrObjAbis: TJavaObjectArray<JString>;
  I: Integer;
  needCheckStatusBarHeight: boolean;
{$ENDIF}
begin
  NavigationBar := 0;
  StatusBar := 0;
{$IFDEF ANDROID}
  if TOSVersion.Major >= 5 then
  begin
    sAbis := '';
    arrObjAbis := TJBuild.JavaClass.SUPPORTED_ABIS;
    for I := 0 to arrObjAbis.Length - 1 do
      sAbis := sAbis + ',' + JStringToString(arrObjAbis.Items[I]);
    sAbis := sAbis.trim([',']);
  end
  else
    sAbis := JStringToString(TJBuild.JavaClass.CPU_ABI) + ',' + JStringToString(TJBuild.JavaClass.CPU_ABI2);

  needCheckStatusBarHeight := sAbis.Contains('x86') or JStringToString(TJBuild.JavaClass.FINGERPRINT).Contains('intel')
    or sAbis.Contains('arm64-v8a');

  if (TOSVersion.Major >= 5) or (needCheckStatusBarHeight) then
  // вроде только работает с 5.0 ниже нет устройства проверить
  begin
    sScale := 1;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenService)) then
      sScale := ScreenService.GetScreenScale; // получаем скейл

    resourceID := TAndroidHelper.Activity.getResources.getIdentifier(StringToJString('status_bar_height'),
      StringToJString('dimen'), StringToJString('android'));
    if resourceID > 0 then
      StatusBar := Trunc(TAndroidHelper.Activity.getResources.getDimensionPixelSize(resourceID) / sScale);

    if hasNavbar(NavigationBar) then
      NavigationBar := Trunc(NavigationBar / sScale);
  end;
{$ENDIF}
end;

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

procedure StatusBarSetColor(const aColor: TAlphaColor);
{$IFDEF ANDROID}
var
  Window: JWindowExt;
{$ENDIF}
begin
{$IFDEF ANDROID}
  CallInUIThread(
    procedure
    begin
      if TOSVersion.Check(5, 0) then
      begin
        Window := GetWindowExt;

        Window.addFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS);
        Window.addFlags(TJWindowManager_LayoutParamsExt.JavaClass.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);

        Window.setFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS,
          TJWindowManager_LayoutParams.JavaClass.FLAG_TRANSLUCENT_STATUS);
        Window.setFlags(TJWindowManager_LayoutParamsExt.JavaClass.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS,
          TJWindowManager_LayoutParamsExt.JavaClass.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);

        Window.setStatusBarColor(TJcolor.JavaClass.TRANSPARENT); // -16777216);
      end;
    end);
{$ENDIF}
{$IFDEF IOS}
  SetStatusBarBackgroundColor(aColor);
{$ENDIF}
end;

end.
