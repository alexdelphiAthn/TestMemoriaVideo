unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Unit2,
  System.Variants, System.Classes, Vcl.Graphics, Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, system.Math, OtlTask, OtlTaskControl, OtlComm;

const
  MSG_LOG      = 1;
  MSG_ERROR    = 2;
  MSG_FINISHED = 3;
  MSG_PROGRESS = 4;
  CHUNK_SIZE_MB = 256;
  CHUNK_SIZE_BYTES = CHUNK_SIZE_MB * 1024 * 1024;
  MSG_START_GPU = 5;
  MSG_STOP_GPU  = 6;

type
  TBlockInfo = record
    Address: PByte;
    Size: NativeUInt;
    Locked: Boolean;
  end;

  TForm1 = class(TForm)
    ProgressBarRAM: TProgressBar;
    EditMegas: TEdit;
    BtnTestRAM: TButton;
    lbl1: TLabel;
    MemoLog: TMemo;
    btnCompleto: TButton;
    BtnFullTest: TButton;
    lbl11: TLabel;
    edtSeconds: TEdit;
    btn1: TButton;
    lbl12: TLabel;
    procedure BtnTestRAMClick(Sender: TObject);
    procedure btnCompletoClick(Sender: TObject);
    procedure BtnFullTestClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    FGPUStressForm: TFormGPUStress;
    procedure EjecutarPruebaRAM(Megas: Integer);
    function AdjustWorkingSet(SizeNeeded: NativeUInt): Boolean;
    procedure EjecutarPruebaConPausa(SecondsDelay: Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.AdjustWorkingSet(SizeNeeded: NativeUInt): Boolean;
var
  MinSize, MaxSize: NativeUInt;
begin
  Result := False;
  // Obtener límites actuales
  if GetProcessWorkingSetSize(GetCurrentProcess, MinSize, MaxSize) then
  begin
    // Si el máximo actual es menor a lo que necesitamos, pedimos más
    if MaxSize < SizeNeeded then
    begin
      // Pedimos un poco más del tamaño necesario (ej. +50MB de margen)
      MaxSize := SizeNeeded + (50 * 1024 * 1024);
      // El mínimo también hay que subirlo a veces, pero con el máximo suele bastar.
      // Intentamos aplicar el cambio
      if SetProcessWorkingSetSize(GetCurrentProcess, MinSize, MaxSize) then
        Result := True;
    end
    else
      Result := True; // Ya teníamos permiso suficiente
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  FormStress: TFormGPUStress;
begin
  // Creamos el formulario. 'Self' hace que si la app se cierra, la ventana también.
  // Pero lo ideal es que se gestione a sí misma.
  FormStress := TFormGPUStress.Create(Application);

  // Solo lo mostramos. El Timer interno se encarga de llamar a RenderFrame.
  FormStress.Show;
end;

procedure TForm1.btnCompletoClick(Sender: TObject);
begin
  // 1. Limpieza inicial de la GUI
  MemoLog.Lines.Clear;
  ProgressBarRAM.Position := 0;
  MemoLog.Lines.Add('Inicializando motor de prueba...');

  CreateTask(
    procedure(const task: IOmniTask)
    var
      MemStatus: TMemoryStatusEx;
      TargetBytes, TotalAllocated: UInt64;
      BytesToAlloc: NativeUInt;
      Blocks: TList<TBlockInfo>;
      NewBlock: TBlockInfo;
      P: PByte;

      // Control de bucles
      iBlock: Integer;
      CurrentInfo: TBlockInfo;
      CurrentPtr: PByte;
      k: NativeUInt;
      Errores: Integer;

      // Matemáticas para la barra de progreso
      TotalBytesToTest: UInt64;
      BytesProcessedGlobal: UInt64;
      ProgressStep: UInt64;
      NextUpdate: UInt64;
    begin
      Errores := 0;
      Blocks := TList<TBlockInfo>.Create;

      try
        // ---------------------------------------------------------
        // PASO 1: DETECCIÓN Y CÁLCULO
        // ---------------------------------------------------------
        MemStatus.dwLength := SizeOf(MemStatus);
        GlobalMemoryStatusEx(MemStatus);

        // Intentamos usar toda la RAM libre menos 512MB de seguridad
        if MemStatus.ullAvailPhys > (512 * 1024 * 1024) then
          TargetBytes := MemStatus.ullAvailPhys - (512 * 1024 * 1024)
        else
          TargetBytes := 100 * 1024 * 1024; // Fallback para equipos con poca RAM

        task.Comm.Send(MSG_LOG, Format('RAM Física Libre: %d MB', [MemStatus.ullAvailPhys div (1024*1024)]));
        task.Comm.Send(MSG_LOG, Format('Meta de prueba: Llenar %d MB', [TargetBytes div (1024*1024)]));

        // Pedir permisos al SO para bloquear esa cantidad
        if AdjustWorkingSet(TargetBytes) then
          task.Comm.Send(MSG_LOG, 'Permisos de WorkingSet obtenidos.')
        else
          task.Comm.Send(MSG_LOG, 'Nota: No se pudo expandir el WorkingSet. VirtualLock podría fallar.');

        // ---------------------------------------------------------
        // PASO 2: RESERVA DE BLOQUES (Allocation)
        // ---------------------------------------------------------
        TotalAllocated := 0;
        task.Comm.Send(MSG_LOG, 'Reservando bloques de memoria...');

        while TotalAllocated < TargetBytes do
        begin
          if (TargetBytes - TotalAllocated) >= CHUNK_SIZE_BYTES then
            BytesToAlloc := CHUNK_SIZE_BYTES
          else
            BytesToAlloc := TargetBytes - TotalAllocated;

          if BytesToAlloc = 0 then Break;

          // VirtualAlloc
          P := VirtualAlloc(nil, BytesToAlloc, MEM_COMMIT, PAGE_READWRITE);

          if P = nil then
          begin
            task.Comm.Send(MSG_LOG, 'Memoria llena o fragmentada. Deteniendo reserva.');
            Break;
          end;

          // Configurar bloque
          NewBlock.Address := P;
          NewBlock.Size := BytesToAlloc;
          // Intentar bloquear en RAM física
          NewBlock.Locked := VirtualLock(P, BytesToAlloc);

          Blocks.Add(NewBlock);
          TotalAllocated := TotalAllocated + BytesToAlloc;

          // Reportar cada 4 bloques para no saturar el log
          if (Blocks.Count mod 4 = 0) then
             task.Comm.Send(MSG_LOG, Format('... Reservados %d MB', [TotalAllocated div (1024*1024)]));

          if task.Terminated then Exit;
        end;

        task.Comm.Send(MSG_LOG, Format('>>> TOTAL RESERVADO: %d MB en %d bloques.', [TotalAllocated div (1024*1024), Blocks.Count]));

        if Blocks.Count = 0 then
        begin
          task.Comm.Send(MSG_ERROR, 'Error Crítico: No se pudo reservar memoria.');
          Exit;
        end;

        // ---------------------------------------------------------
        // CONFIGURACIÓN DE BARRA DE PROGRESO
        // ---------------------------------------------------------
        // El trabajo total es: (Bytes totales * 2 pasadas)
        TotalBytesToTest := TotalAllocated * 2;
        BytesProcessedGlobal := 0;

        // Actualizar barra cada 1%
        ProgressStep := TotalBytesToTest div 100;
        if ProgressStep = 0 then ProgressStep := 1024 * 1024;
        NextUpdate := ProgressStep;

        // ---------------------------------------------------------
        // PASO 3: FASE 0xAA (Escritura y Verificación)
        // ---------------------------------------------------------
        task.Comm.Send(MSG_LOG, '--- INICIO FASE 1: Patrón 0xAA ---');

        for iBlock := 0 to Blocks.Count - 1 do
        begin
           CurrentInfo := Blocks[iBlock];

           // Llenado rápido
           FillChar(CurrentInfo.Address^, CurrentInfo.Size, $AA);

           // Verificación byte a byte
           CurrentPtr := CurrentInfo.Address;
           for k := 0 to CurrentInfo.Size - 1 do
           begin
             if CurrentPtr^ <> $AA then
             begin
               Inc(Errores);
               task.Comm.Send(MSG_ERROR, Format('FALLO en Bloque %d Offset %d. Esperado AA, Leído %x', [iBlock, k, CurrentPtr^]));
               Break;
             end;
             Inc(CurrentPtr);

             // Actualizar Barra
             Inc(BytesProcessedGlobal);
             if BytesProcessedGlobal >= NextUpdate then
             begin
               task.Comm.Send(MSG_PROGRESS, Round((BytesProcessedGlobal / TotalBytesToTest) * 100));
               Inc(NextUpdate, ProgressStep);
               if task.Terminated then Exit;
             end;
           end;
           if Errores > 0 then Break;
        end;

        if Errores = 0 then
        begin
          // ---------------------------------------------------------
          // PASO 4: FASE 0x55 (Escritura y Verificación)
          // ---------------------------------------------------------
          task.Comm.Send(MSG_LOG, '--- INICIO FASE 2: Patrón 0x55 ---');

          for iBlock := 0 to Blocks.Count - 1 do
          begin
             CurrentInfo := Blocks[iBlock];

             FillChar(CurrentInfo.Address^, CurrentInfo.Size, $55);

             CurrentPtr := CurrentInfo.Address;
             for k := 0 to CurrentInfo.Size - 1 do
             begin
               if CurrentPtr^ <> $55 then
               begin
                 Inc(Errores);
                 task.Comm.Send(MSG_ERROR, Format('FALLO en Bloque %d Offset %d. Esperado 55, Leído %x', [iBlock, k, CurrentPtr^]));
                 Break;
               end;
               Inc(CurrentPtr);

               // Actualizar Barra
               Inc(BytesProcessedGlobal);
               if BytesProcessedGlobal >= NextUpdate then
               begin
                 task.Comm.Send(MSG_PROGRESS, Round((BytesProcessedGlobal / TotalBytesToTest) * 100));
                 Inc(NextUpdate, ProgressStep);
                 if task.Terminated then Exit;
               end;
             end;
             if Errores > 0 then Break;
          end;
        end;

      finally
        // ---------------------------------------------------------
        // PASO 5: LIMPIEZA
        // ---------------------------------------------------------
        task.Comm.Send(MSG_LOG, 'Liberando recursos...');
        for iBlock := 0 to Blocks.Count - 1 do
        begin
          CurrentInfo := Blocks[iBlock];
          if CurrentInfo.Locked then
            VirtualUnlock(CurrentInfo.Address, CurrentInfo.Size);

          VirtualFree(CurrentInfo.Address, 0, MEM_RELEASE);
        end;
        Blocks.Free;
      end;

      // Reporte Final
      if Errores = 0 then
        task.Comm.Send(MSG_FINISHED, Format('¡PRUEBA EXITOSA! Se verificaron %d MB sin errores.', [TotalAllocated div (1024*1024)]))
      else
        task.Comm.Send(MSG_FINISHED, 'LA PRUEBA FALLÓ. Revisa el log para detalles.');

    end)
    .OnMessage(procedure(const task: IOmniTaskControl; const msg: TOmniMessage)
      begin
        case msg.MsgID of
          MSG_LOG:
            begin
              MemoLog.Lines.Add(msg.MsgData.AsString);
              SendMessage(MemoLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;

          MSG_ERROR:
            begin
              MemoLog.Lines.Add('!!! ' + msg.MsgData.AsString);
              SendMessage(MemoLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;

          MSG_PROGRESS: ProgressBarRAM.Position := msg.MsgData.AsInteger;

          // --- NUEVO: ABRIR VENTANA GPU ---
          MSG_START_GPU:
            begin
              // Si no existe, la creamos
              if not Assigned(FGPUStressForm) then
                FGPUStressForm := TFormGPUStress.Create(Self);

              FGPUStressForm.Show; // Mostrar sin bloquear (Modeless)
              FGPUStressForm.Update; // Forzar pintado inmediato
            end;

          // --- NUEVO: CERRAR VENTANA GPU ---
          MSG_STOP_GPU:
            begin
              if Assigned(FGPUStressForm) then
              begin
                FGPUStressForm.Close;
                FGPUStressForm.Free;
                FGPUStressForm := nil; // Marcar como nulo para evitar errores
              end;
            end;

          MSG_FINISHED:
            begin
              // Por seguridad, aseguramos que la GPU se apague si el test acaba
              if Assigned(FGPUStressForm) then
              begin
                FGPUStressForm.Free;
                FGPUStressForm := nil;
              end;

              ProgressBarRAM.Position := 100;
              ShowMessage(msg.MsgData.AsString);
              BtnFullTest.Enabled := True;
            end;
        end;
      end)
    .Run;
end;

procedure TForm1.BtnFullTestClick(Sender: TObject);
var
  Secs: Integer;
begin
  if not TryStrToInt(edtSeconds.Text, Secs) then Secs := 0;
  BtnFullTest.Enabled := False;
  EjecutarPruebaConPausa(Secs);
end;

procedure TForm1.BtnTestRAMClick(Sender: TObject);
var
  MegasInput: Integer;
begin
  if not TryStrToInt(EditMegas.Text, MegasInput) then
  begin
    ShowMessage('Número inválido.');
    Exit;
  end;

  if MegasInput <= 0 then Exit;

  // Deshabilitar botón para evitar doble clic accidental
  BtnTestRAM.Enabled := False;
  try
    EjecutarPruebaRAM(MegasInput);
  finally
    // Nota: Como OTL es asíncrono, lo ideal es reactivar el botón
    // en el mensaje MSG_FINISHED o MSG_ERROR, no aquí inmediatamente.
    // Pero para pruebas rápidas, puedes dejarlo o gestionar el estado del botón en el OnMessage.
    BtnTestRAM.Enabled := True;
  end;
end;

procedure TForm1.EjecutarPruebaRAM(Megas: Integer);
begin
  MemoLog.Lines.Clear;
  ProgressBarRAM.Position := 0; // Resetear barra
  MemoLog.Lines.Add(Format('Iniciando prueba para %d MB...', [Megas]));

  CreateTask(
    procedure(const task: IOmniTask)
    var
      SizeMB: Integer;
      SizeBytes: NativeUInt;
      P: PByte;
      Current: PByte;
      i: NativeUInt;
      Errores: Integer;

      // Variables para control de progreso
      ProgressStep: NativeUInt;
      NextProgressUpdate: NativeUInt;
      BasePercent: Integer;
    begin
      SizeMB := task.Param['Megas'].AsInteger;
      SizeBytes := NativeUInt(SizeMB) * 1024 * 1024;
      Errores := 0;

      // Calcular cada cuántos bytes actualizamos la barra (ej: cada 1% del total)
      // Esto evita enviar millones de mensajes y saturar la GUI
      ProgressStep := SizeBytes div 100;
      if ProgressStep = 0 then ProgressStep := 1024; // Mínimo de seguridad
      if not AdjustWorkingSet(SizeBytes) then
        task.Comm.Send(MSG_LOG, 'Advertencia: No se pudo ampliar el Working Set. VirtualLock podría fallar.');
      task.Comm.Send(MSG_LOG, Format('Reservando %d bytes...', [SizeBytes]));
      P := VirtualAlloc(nil, SizeBytes, MEM_COMMIT, PAGE_READWRITE);

      if P = nil then
      begin
        task.Comm.Send(MSG_ERROR, 'Fallo crítico: No se pudo reservar memoria.');
        Exit;
      end;

      try
        if not VirtualLock(P, SizeBytes) then
          task.Comm.Send(MSG_LOG, 'Nota: No se pudo bloquear RAM física (Swap activo).');

        // ---------------------------------------------------------
        // FASE 1: Patrón 0xAA
        // ---------------------------------------------------------
        if task.Terminated then Exit;
        task.Comm.Send(MSG_LOG, 'Escribiendo 0xAA...');
        // FillChar es muy rápido, no necesitamos barra aquí
        FillChar(P^, SizeBytes, $AA);

        task.Comm.Send(MSG_LOG, 'Verificando 0xAA...');
        Current := P;
        NextProgressUpdate := ProgressStep;

        // Bucle de verificación FASE 1 (0% a 50% de la barra)
        for i := 0 to SizeBytes - 1 do
        begin
          if Current^ <> $AA then
          begin
            Inc(Errores);
            task.Comm.Send(MSG_ERROR, Format('Error en byte %d (Fase AA). Valor: %x', [i, Current^]));
            Break;
          end;

          // Actualizar Progreso
          if i >= NextProgressUpdate then
          begin
            // Calculamos porcentaje relativo (0-50)
            // i / SizeBytes da 0..1, multiplicamos por 50.
            task.Comm.Send(MSG_PROGRESS, Round((i / SizeBytes) * 50));
            Inc(NextProgressUpdate, ProgressStep);

            if task.Terminated then Exit;
          end;

          Inc(Current);
        end;

        if Errores > 0 then Exit;

        // ---------------------------------------------------------
        // FASE 2: Patrón 0x55
        // ---------------------------------------------------------
        if task.Terminated then Exit;
        task.Comm.Send(MSG_LOG, 'Escribiendo 0x55...');
        FillChar(P^, SizeBytes, $55);

        task.Comm.Send(MSG_LOG, 'Verificando 0x55...');
        Current := P;
        NextProgressUpdate := ProgressStep;

        // Bucle de verificación FASE 2 (50% a 100% de la barra)
        for i := 0 to SizeBytes - 1 do
        begin
          if Current^ <> $55 then
          begin
            Inc(Errores);
            task.Comm.Send(MSG_ERROR, Format('Error en byte %d (Fase 55). Valor: %x', [i, Current^]));
            Break;
          end;

          // Actualizar Progreso
          if i >= NextProgressUpdate then
          begin
            // Calculamos porcentaje relativo (50-100)
            // 50 + (i / SizeBytes * 50)
            task.Comm.Send(MSG_PROGRESS, 50 + Round((i / SizeBytes) * 50));
            Inc(NextProgressUpdate, ProgressStep);

            if task.Terminated then Exit;
          end;

          Inc(Current);
        end;

        // Asegurar que llegue al 100% visualmente al acabar
        task.Comm.Send(MSG_PROGRESS, 100);

      finally
        VirtualUnlock(P, SizeBytes);
        VirtualFree(P, 0, MEM_RELEASE);
      end;

      if Errores = 0 then
        task.Comm.Send(MSG_FINISHED, 'Prueba finalizada SIN ERRORES.')
      else
        task.Comm.Send(MSG_FINISHED, 'Prueba finalizada con FALLOS.');
    end)
    .SetParameter('Megas', Megas)
    .OnMessage(
      procedure(const task: IOmniTaskControl; const msg: TOmniMessage)
      begin
        case msg.MsgID of
          MSG_LOG:      MemoLog.Lines.Add('Info: ' + msg.MsgData.AsString);
          MSG_ERROR:    MemoLog.Lines.Add('ERROR: ' + msg.MsgData.AsString);

          // ---- MANEJO DEL PROGRESO ----
          MSG_PROGRESS: ProgressBarRAM.Position := msg.MsgData.AsInteger;

          MSG_FINISHED:
            begin
              ProgressBarRAM.Position := 100; // Asegurar barra llena
              ShowMessage(msg.MsgData.AsString);
            end;
        end;
      end)
    .Run;
end;

procedure TForm1.EjecutarPruebaConPausa(SecondsDelay: Integer);
begin
  MemoLog.Lines.Clear;
  ProgressBarRAM.Position := 0;
  MemoLog.Lines.Add(Format('Iniciando prueba de degradación con espera de %d seg...', [SecondsDelay]));

  CreateTask(
    procedure(const task: IOmniTask)
    var
      MemStatus: TMemoryStatusEx;
      TargetBytes, TotalAllocated: UInt64;
      BytesToAlloc: NativeUInt;
      Blocks: TList<TBlockInfo>;
      NewBlock: TBlockInfo;
      P: PByte;
      iBlock: Integer;
      CurrentInfo: TBlockInfo;
      CurrentPtr: PByte;
      k: NativeUInt;
      Errores: Integer;
      TotalBytesToTest: UInt64; // Bytes totales de trabajo (Escritura + Lectura)
      BytesProcessedGlobal: UInt64;
      ProgressStep: UInt64;
      NextUpdate: UInt64;

      // Procedimiento interno para la pausa cancelable
// Procedimiento interno modificado
      procedure Esperar(Segundos: Integer);
      var
        WaitMS, Elapsed: Integer;
      begin
        if Segundos <= 0 then Exit;
        task.Comm.Send(MSG_LOG, '--- INICIANDO ESTRÉS DE GPU + PAUSA ---');
        // 1. ORDENAR AL HILO PRINCIPAL QUE ABRA LA VENTANA GPU
        task.Comm.Send(MSG_START_GPU);
        WaitMS := Segundos * 1000;
        Elapsed := 0;
        // Bucle de espera (mientras la GPU está a tope en la otra ventana)
        while Elapsed < WaitMS do
        begin
          Sleep(100);
          Inc(Elapsed, 100);
          if task.Terminated then
          begin
             task.Comm.Send(MSG_STOP_GPU); // Asegurar apagado si cancelan
             Abort;
          end;
        end;
        // 2. ORDENAR AL HILO PRINCIPAL QUE CIERRE LA VENTANA GPU
        task.Comm.Send(MSG_STOP_GPU);
        task.Comm.Send(MSG_LOG, '--- FIN PAUSA/GPU. Verificando RAM... ---');
        // Pequeña espera técnica para que el voltaje se estabilice tras parar la GPU
        Sleep(500);
      end;

    begin
      Errores := 0;
      Blocks := TList<TBlockInfo>.Create;
      try
        // 1. DETECCIÓN
        MemStatus.dwLength := SizeOf(MemStatus);
        GlobalMemoryStatusEx(MemStatus);
        if MemStatus.ullAvailPhys > (512 * 1024 * 1024) then
          TargetBytes := MemStatus.ullAvailPhys - (512 * 1024 * 1024)
        else
          TargetBytes := 100 * 1024 * 1024;
        AdjustWorkingSet(TargetBytes);
        // 2. RESERVA
        TotalAllocated := 0;
        task.Comm.Send(MSG_LOG, 'Reservando memoria...');
        while TotalAllocated < TargetBytes do
        begin
          if (TargetBytes - TotalAllocated) >= CHUNK_SIZE_BYTES then
            BytesToAlloc := CHUNK_SIZE_BYTES
          else
            BytesToAlloc := TargetBytes - TotalAllocated;
          if BytesToAlloc = 0 then
            Break;
          P := VirtualAlloc(nil, BytesToAlloc, MEM_COMMIT, PAGE_READWRITE);
          if P = nil then
            Break;
          NewBlock.Address := P;
          NewBlock.Size := BytesToAlloc;
          NewBlock.Locked := VirtualLock(P, BytesToAlloc);
          Blocks.Add(NewBlock);
          TotalAllocated := TotalAllocated + BytesToAlloc;
          if task.Terminated then Exit;
        end;
        task.Comm.Send(MSG_LOG, Format('Memoria reservada: %d MB', [TotalAllocated div (1024*1024)]));
        // CALCULO BARRA PROGRESO
        // Total = (EscribirAA + LeerAA + Escribir55 + Leer55)
        TotalBytesToTest := TotalAllocated * 4;
        BytesProcessedGlobal := 0;
        ProgressStep := TotalBytesToTest div 100;
        if ProgressStep = 0 then ProgressStep := 1024*1024;
        NextUpdate := ProgressStep;
        // =========================================================
        // FASE 1: PATRÓN 0xAA
        // =========================================================
        // A) ESCRITURA MASIVA (Llenamos TODO primero)
        task.Comm.Send(MSG_LOG, 'Escribiendo 0xAA en toda la memoria...');
        for iBlock := 0 to Blocks.Count - 1 do
        begin
           CurrentInfo := Blocks[iBlock];
           FillChar(CurrentInfo.Address^, CurrentInfo.Size, $AA);
           BytesProcessedGlobal := BytesProcessedGlobal + CurrentInfo.Size;
           if BytesProcessedGlobal >= NextUpdate then
           begin
             task.Comm.Send(MSG_PROGRESS, Round((BytesProcessedGlobal / TotalBytesToTest) * 100));
             Inc(NextUpdate, ProgressStep);
           end;
           if task.Terminated then Exit;
        end;
        // B) PAUSA
        try
          Esperar(task.Param['Delay'].AsInteger);
        except
          Exit;
        end;
        // C) LECTURA MASIVA
        task.Comm.Send(MSG_LOG, 'Verificando 0xAA...');
        for iBlock := 0 to Blocks.Count - 1 do
        begin
           CurrentInfo := Blocks[iBlock];
           CurrentPtr := CurrentInfo.Address;
           for k := 0 to CurrentInfo.Size - 1 do
           begin
             if CurrentPtr^ <> $AA then
             begin
               Inc(Errores);
               task.Comm.Send(MSG_ERROR, Format('ERROR BIT ROT: Bloque %d Byte %d. Esperado AA, Leído %x', [iBlock, k, CurrentPtr^]));
               Break;
             end;
             Inc(CurrentPtr);
           end;
           if Errores > 0 then
             Break;
           BytesProcessedGlobal := BytesProcessedGlobal + CurrentInfo.Size;
           if BytesProcessedGlobal >= NextUpdate then
           begin
             task.Comm.Send(MSG_PROGRESS, Round((BytesProcessedGlobal / TotalBytesToTest) * 100));
             Inc(NextUpdate, ProgressStep);
           end;
           if task.Terminated then
             Exit;
        end;
        if Errores > 0 then
          Exit; // Salir si ya falló
        // =========================================================
        // FASE 2: PATRÓN 0x55
        // =========================================================
        // A) ESCRITURA MASIVA
        task.Comm.Send(MSG_LOG, 'Escribiendo 0x55 en toda la memoria...');
        for iBlock := 0 to Blocks.Count - 1 do
        begin
           CurrentInfo := Blocks[iBlock];
           FillChar(CurrentInfo.Address^, CurrentInfo.Size, $55);

           BytesProcessedGlobal := BytesProcessedGlobal + CurrentInfo.Size;
           if BytesProcessedGlobal >= NextUpdate then
           begin
             task.Comm.Send(MSG_PROGRESS, Round((BytesProcessedGlobal / TotalBytesToTest) * 100));
             Inc(NextUpdate, ProgressStep);
           end;
           if task.Terminated then Exit;
        end;
        // B) PAUSA
        try
          Esperar(task.Param['Delay'].AsInteger);
        except
          Exit;
        end;
        // C) LECTURA MASIVA
        task.Comm.Send(MSG_LOG, 'Verificando 0x55...');
        for iBlock := 0 to Blocks.Count - 1 do
        begin
           CurrentInfo := Blocks[iBlock];
           CurrentPtr := CurrentInfo.Address;
           for k := 0 to CurrentInfo.Size - 1 do
           begin
             if CurrentPtr^ <> $55 then
             begin
               Inc(Errores);
               task.Comm.Send(MSG_ERROR, Format('ERROR BIT ROT: Bloque %d Byte %d. Esperado 55, Leído %x', [iBlock, k, CurrentPtr^]));
               Break;
             end;
             Inc(CurrentPtr);
           end;
           if (Errores > 0) then
             Break;
           BytesProcessedGlobal := BytesProcessedGlobal + CurrentInfo.Size;
           if BytesProcessedGlobal >= NextUpdate then
           begin
             task.Comm.Send(MSG_PROGRESS, Round((BytesProcessedGlobal / TotalBytesToTest) * 100));
             Inc(NextUpdate, ProgressStep);
           end;
           if task.Terminated then Exit;
        end;

      finally
        task.Comm.Send(MSG_LOG, 'Liberando RAM...');
        for iBlock := 0 to Blocks.Count - 1 do
        begin
          CurrentInfo := Blocks[iBlock];
          if CurrentInfo.Locked then VirtualUnlock(CurrentInfo.Address, CurrentInfo.Size);
          VirtualFree(CurrentInfo.Address, 0, MEM_RELEASE);
        end;
        Blocks.Free;
      end;
      if Errores = 0 then
        task.Comm.Send(MSG_FINISHED, 'Prueba de integridad finalizada SIN ERRORES.')
      else
        task.Comm.Send(MSG_FINISHED, 'SE ENCONTRARON ERRORES DE HARDWARE.');
    end)
    .SetParameter('Delay', SecondsDelay) // Pasamos el parámetro
    .OnMessage(procedure(const task: IOmniTaskControl; const msg: TOmniMessage)
      begin
        case msg.MsgID of
          MSG_LOG:
            begin
              MemoLog.Lines.Add(msg.MsgData.AsString);
              SendMessage(MemoLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;

          MSG_ERROR:
            begin
              MemoLog.Lines.Add('!!! ' + msg.MsgData.AsString);
              SendMessage(MemoLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;

          MSG_PROGRESS:
            ProgressBarRAM.Position := msg.MsgData.AsInteger;

          // --- CORRECCIÓN AQUÍ ---
          MSG_START_GPU:
            begin
              // 1. Usar Application como dueño (igual que en tu btn1)
              if not Assigned(FGPUStressForm) then
                FGPUStressForm := TFormGPUStress.Create(Application);

              // 2. Mostrar la ventana
              FGPUStressForm.Show;

              // 3. ¡OBLIGATORIO! Traer al frente y repintar
              FGPUStressForm.BringToFront;
              FGPUStressForm.Repaint;
            end;

          MSG_STOP_GPU:
            begin
              if Assigned(FGPUStressForm) then
              begin
                // Usamos Free directamente para cerrar y liberar memoria a la vez
                // Esto evita conflictos si el OnClose tiene Action := caFree
                FGPUStressForm.Free;
                FGPUStressForm := nil;
              end;
            end;
          // -----------------------

          MSG_FINISHED:
            begin
              // Limpieza de seguridad por si acaso
              if Assigned(FGPUStressForm) then
              begin
                FGPUStressForm.Free;
                FGPUStressForm := nil;
              end;

              ProgressBarRAM.Position := 100;
              ShowMessage(msg.MsgData.AsString);
              BtnFullTest.Enabled := True;
            end;
        end;
      end)
      .Run;
end;

end.
