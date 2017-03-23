unit uMain;

{
  author : ZuBy
  https://github.com/rzaripov1990
  rzaripov1990@gmail.com

  Чтобы достичь нужного эффекта,
  все контролы нужно ложить в Content (TRectangle)
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Colors;

type
  TFormMain = class(TForm)
    Content: TRectangle;
    ColorPanel1: TColorPanel;
    ColorBox1: TColorBox;
    Label1: TLabel;
    procedure FormResize(Sender: TObject);
    procedure ColorPanel1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure RebuildOrientation;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  FMX.StatusBar;

procedure TFormMain.ColorPanel1Change(Sender: TObject);
begin
  TmyWindow.StatusBarColor(Self, ColorBox1.Color);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  RebuildOrientation;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  ColorPanel1Change(nil);
end;

procedure TFormMain.RebuildOrientation;
begin
  Content.Margins.Top := TmyWindow.StatusBarHeight;
end;

initialization

TmyWindow.Init;

end.
