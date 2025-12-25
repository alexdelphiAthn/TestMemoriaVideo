# ⚡ RAM Soak & GPU Stress Tester (Delphi)

**Una herramienta de diagnóstico avanzada para Windows x64 diseñada para detectar "Bit Rot" y fallos de hardware mediante estrés combinado (RAM + GPU).**

![Platform](https://img.shields.io/badge/Platform-Windows%20x64-blue) ![Language](https://img.shields.io/badge/Language-Delphi%20(Pascal)-red) ![License](https://img.shields.io/badge/License-MIT-green)

## 📖 Descripción

A diferencia de los tests de memoria tradicionales (como MemTest86) que verifican la memoria de forma aislada, esta herramienta busca fallos que solo ocurren bajo **carga eléctrica compleja**.

El programa implementa una estrategia de **"Llenado Total"** con una fase de **"Maceración" (Soak Test)**:
1. Llena la RAM física casi al 100% con patrones de bits específicos.
2. Entra en una pausa configurable (tiempo de retención).
3. **Durante la pausa, activa un estrés de GPU (VRAM/VRM)** mediante Direct2D para generar ruido eléctrico (ripple) en la placa base.
4. Verifica si los datos en RAM han mutado (Bit Rot) debido a la interferencia o condensadores defectuosos.

## 🚀 Características Clave

* **Algoritmo de Llenado Inteligente:** Utiliza una lista de bloques (`Chunks`) para fragmentar y reservar la máxima cantidad de RAM física disponible, evitando el archivo de paginación (Swap) mediante `VirtualLock`.
* **Patrones de Estrés:** Utiliza patrones alternos `0xAA` (10101010) y `0x55` (01010101) para forzar el cambio de estado de todas las celdas.
* **Modo "Tormenta Perfecta":**
    * Permite definir un tiempo de espera entre escritura y lectura (ej. 60 segundos).
    * Lanza una ventana de renderizado **Direct2D** acelerada por hardware que genera patrones estroboscópicos y geometría aleatoria a 60FPS.
    * Esto estresa el regulador de voltaje de la placa base mientras la RAM intenta retener la carga.
* **Multihilo:** Basado en **OmniThreadLibrary (OTL)** para no congelar la interfaz de usuario durante las operaciones intensivas.
* **Soporte Large Address Aware:** Diseñado para compilarse en **64-bits**, permitiendo probar 8GB, 16GB, 32GB o más.

## 🛠️ Requisitos de Compilación

* **IDE:** Delphi 10.4 Sydney, Delphi 11 Alexandria o superior.
* **Librerías:**
    * [OmniThreadLibrary](https://github.com/gabr42/OmniThreadLibrary) (Obligatorio para el manejo de hilos).
* **Plataforma Objetivo:** Debe compilarse seleccionando **Windows 64-bit** en el Project Manager.

## 📦 Instalación y Uso

### Escenario Recomendado: Windows Live USB
Para obtener los mejores resultados y probar la mayor cantidad de RAM posible, se recomienda ejecutar este programa desde un entorno ligero como **WinPE**, **Hiren's Boot CD** o **Sergei Strelec** (versiones x64).

### Pasos
1.  Descarga la última *Release* o compila el código fuente.
2.  Ejecuta `RamStressTester.exe` como Administrador.
3.  **Prueba Básica:**
    * Introduce la cantidad de MB o pulsa "Test Completo".
4.  **Prueba de Degradación (Recomendada):**
    * Introduce un tiempo de espera en segundos (ej. `60` o `120`).
    * Pulsa **"Iniciar Test Completo"**.
    * El programa llenará la RAM -> Abrirá la ventana de GPU (pantalla parpadeante) -> Esperará -> Verificará los datos.

## 🔍 Interpretación de Resultados

* **Barra de Progreso:**
    * 0% - 50%: Fase de patrón `0xAA`.
    * 50% - 100%: Fase de patrón `0x55`.
* **Log de Errores:**
    * Si aparece **"ERROR BIT ROT"**, significa que un bit cambió de valor espontáneamente. Esto indica RAM defectuosa o problemas de alimentación en la placa base.
* **Ventana GPU:**
    * Si durante el parpadeo ves **píxeles de colores extraños ("chispas")**, líneas horizontales, o el driver de vídeo se reinicia, la GPU integrada o la RAM compartida tienen fallos.

## ⚠️ Disclaimer

Este software somete al hardware (CPU, RAM y VRMs) a una carga de trabajo intensa.
**Úselo bajo su propia responsabilidad.** El autor no se hace responsable de daños en hardware inestable o con overclocking inestable.

## 📄 Licencia

Este proyecto está bajo la Licencia MIT - ver el archivo [LICENSE.md](LICENSE.md) para más detalles.

---
*Desarrollado con Delphi y OmniThreadLibrary.*
