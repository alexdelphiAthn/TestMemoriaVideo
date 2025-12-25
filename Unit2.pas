unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  // Unidades de Direct2D obligatorias
  Vcl.Direct2D, Winapi.D2D1;

type
  TFormGPUStress = class(TForm)
    tmr1: TTimer;
  private
    FD2DCanvas: TDirect2DCanvas;
    FTimer: TTimer;
    FFrameCount: Integer;

    // Métodos internos
    procedure OnTimerTick(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure RenderFrame;
  protected
    // Usamos OVERRIDE para asegurar que se ejecuten sin vincular eventos
    procedure CreateWnd; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormGPUStress: TFormGPUStress;

implementation

{$R *.dfm}

{ TFormGPUStress }

// 1. CONSTRUCTOR: Aquí garantizamos que todo arranque
constructor TFormGPUStress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Configuración de la ventana
  Caption := 'PRUEBA VISUAL DE GPU (ESTRESANDO VRAM)';
  Color := clBlack;
  BorderStyle := bsNone;
  WindowState := wsMaximized;

  // Crear el Timer manualmente
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 16; // ~60 FPS
  FTimer.OnTimer := OnTimerTick;
  FTimer.Enabled := True;
end;

// 2. DESTRUCTOR: Limpieza segura
destructor TFormGPUStress.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  if Assigned(FD2DCanvas) then FreeAndNil(FD2DCanvas);
  inherited Destroy;
end;

// 3. CREATEWND: Asegurar que el canvas existe cuando la ventana tiene handle
procedure TFormGPUStress.CreateWnd;
begin
  inherited;
  if not Assigned(FD2DCanvas) then
    FD2DCanvas := TDirect2DCanvas.Create(Handle);
end;

// 4. RESIZE: Recrear Canvas si cambia el tamaño (Override)
procedure TFormGPUStress.Resize;
begin
  inherited; // Importante llamar al heredado
  if Assigned(FD2DCanvas) then FreeAndNil(FD2DCanvas);

  // Si la ventana tiene handle, creamos el canvas
  if HandleAllocated then
    FD2DCanvas := TDirect2DCanvas.Create(Handle);
end;

// 5. TIMER: El corazón del repintado
procedure TFormGPUStress.OnTimerTick(Sender: TObject);
begin
  // Forzamos a Windows a pedir un repintado
  Invalidate;
end;

// 6. EVITAR PARPADEO BLANCO
procedure TFormGPUStress.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1; // Decimos "Ya lo borré yo", para que Windows no pinte blanco
end;

// 7. PAINT: Interceptamos el mensaje de pintado
procedure TFormGPUStress.WMPaint(var Message: TWMPaint);
var
  PaintStruct: TPaintStruct;
begin
  BeginPaint(Handle, PaintStruct);
  try
    RenderFrame;
  finally
    EndPaint(Handle, PaintStruct);
  end;
end;

// 8. DIBUJO DIRECT2D (Tu lógica gráfica)
procedure TFormGPUStress.RenderFrame;
var
  i: Integer;
  R: TRect;
  Brush: ID2D1SolidColorBrush;
  SizeX, SizeY: Integer;
  RenderTarget: ID2D1RenderTarget;
  ColorF: D2D1_COLOR_F; // Variable auxiliar para el color
begin
  if not Assigned(FD2DCanvas) then Exit;

  RenderTarget := FD2DCanvas.RenderTarget;

  FD2DCanvas.BeginDraw;
  try
    Inc(FFrameCount);
    SizeX := ClientWidth;
    SizeY := ClientHeight;

    // Fondo Estroboscópico
    if (FFrameCount mod 2) = 0 then
      RenderTarget.Clear(D2D1ColorF(clBlack))
    else
      RenderTarget.Clear(D2D1ColorF(clWhite));

    // Crear Pincel
    RenderTarget.CreateSolidColorBrush(D2D1ColorF(clRed), nil, Brush);

    // Dibujar Rectángulos
    for i := 0 to 1000 do
    begin
      // Configurar color aleatorio manualmente
      ColorF.r := Random;
      ColorF.g := Random;
      ColorF.b := Random;
      ColorF.a := 1.0; // Opaco

      Brush.SetColor(ColorF);

      R := Rect(Random(SizeX), Random(SizeY), Random(SizeX) + 150, Random(SizeY) + 150);

      RenderTarget.FillRectangle(D2D1RectF(R.Left, R.Top, R.Right, R.Bottom), Brush);
    end;

  finally
    try
      FD2DCanvas.EndDraw;
    except
      // Si la GPU se reinicia o se pierde el dispositivo, evitamos el crash
      FreeAndNil(FD2DCanvas);
    end;
  end;
end;

end.
