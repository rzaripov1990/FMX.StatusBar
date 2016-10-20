unit uMain;

{
  author : ZuBy
  https://github.com/rzaripov1990
  rzaripov1990@gmail.com

  Чтобы достичь нужного эффекта,
  все контролы нужно ложить в Content (TRectangle)

  ANDROID:
  Version Info -> theme = No Title
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Colors, FMX.Platform;

type
  TmyUI = record
    StatusBar, NavBar: Integer;
    class function ContentColor: TAlphaColor; static;
  end;

  TFormMain = class(TForm)
    Content: TRectangle;
    ColorPanel1: TColorPanel;
    chUseBlackNavBar: TCheckBox;
    rNavBar: TRectangle;
    ColorBox1: TColorBox;
    Label1: TLabel;
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ColorPanel1Change(Sender: TObject);
    procedure chUseBlackNavBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure RebuildContent;
    procedure RebuildOrientation;
  public
    { Public declarations }
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  end;

var
  FormMain: TFormMain;
  myUI: TmyUI;

implementation

{$R *.fmx}

uses FMX.StatusBar;

{ TmyUI }

class function TmyUI.ContentColor: TAlphaColor;
begin
  TAlphaColorRec(Result).R := 238;
  TAlphaColorRec(Result).G := 238;
  TAlphaColorRec(Result).B := 238;
  TAlphaColorRec(Result).A := 255;
end;

procedure TFormMain.chUseBlackNavBarChange(Sender: TObject);
begin
  rNavBar.Visible := chUseBlackNavBar.IsChecked;
  RebuildOrientation
end;

procedure TFormMain.ColorPanel1Change(Sender: TObject);
begin
  Fill.Color := ColorBox1.Color; // задаем цвет для всей формы
{$IFDEF IOS} StatusBarSetColor(Fill.Color); {$ENDIF} // смена цвета в run-time для IOS
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  Content.Fill.Color := TmyUI.ContentColor; // задаем цвет для нашего контейнера
  Fill.Kind := TBrushKind.Solid; // режим покраски формы
  Fill.Color := ColorBox1.Color; // задаем цвет для всей формы
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  aFMXApplicationEventService: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService,
    IInterface(aFMXApplicationEventService)) then
    aFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  RebuildOrientation;
end;

function TFormMain.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.BecameActive:
      RebuildContent;
  end;
  Result := True;
end;

procedure TFormMain.RebuildContent;
begin
  StatusBarGetBounds(myUI.StatusBar, myUI.NavBar); // получаем отступы
  StatusBarSetColor(Fill.Color);
  { на андроиде это работает так:
    //  форма становится на полные размеры (типа FullScreen режим)
    //  статус бар становится полупрозрачным, поэтому мы видим часть нашей формы
    //  и если эту часть формы покрасить, то достигается нужный эффект
  }
  { на айос это работает так:
    // статус бар принимает цвет формы, но если в run-time менять цвет, то он не меняется сразу
    // поэтому нужно вызвать снова этот метод, для моментальной смены цвета
  }
end;

procedure TFormMain.RebuildOrientation;
var
  aVert: Boolean;
begin
  aVert := Height > Width; // получаем ориентацию
  if aVert then
  begin
    Content.Margins.Left := 0;
    Content.Margins.Top := myUI.StatusBar; // оступ сверху, для андроид 5.0+
    Content.Margins.Right := 0;
    if chUseBlackNavBar.IsChecked then
    begin
      rNavBar.Align := TAlignLayout.MostBottom;
      rNavBar.Height := myUI.NavBar;
      Content.Margins.Bottom := 0; // убираем отступ
    end
    else
      Content.Margins.Bottom := myUI.NavBar; // отспут снизу, для андроид 5.0+ (при наличии наэкранных кнопок)
  end
  else
  begin
    if not NavBarOrientationBottom then
    begin
      // в горизонтальной ориентции, отспут будет справа
      Content.Margins.Left := 0;
      Content.Margins.Top := myUI.StatusBar; // оступ сверху, для андроид 5.0+
      if chUseBlackNavBar.IsChecked then
      begin
        rNavBar.Align := TAlignLayout.MostRight;
        rNavBar.Width := myUI.NavBar;
        Content.Margins.Right := 0; // убираем отступ
      end
      else
        Content.Margins.Right := myUI.NavBar; // отспут справа, для андроид 5.0+ (при наличии наэкранных кнопок)
      Content.Margins.Bottom := 0;
    end;
  end;
end;

end.
