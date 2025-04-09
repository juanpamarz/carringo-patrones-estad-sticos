import serial
import csv

# Configura el puerto serie (ajusta el puerto según tu sistema, ej. COM3 en Windows o /dev/ttyUSB0 en Linux)
puerto = "COM5"  # Cambia según corresponda
baudrate = 9600

# Abre la conexión serial
ser = serial.Serial(puerto, baudrate, timeout=1)

# Crea el archivo CSV y escribe el encabezado
with open("datos5.csv", "w", newline="") as archivo_csv:
    writer = csv.writer(archivo_csv)
    writer.writerow(["Tiempo (ms)", "Distancia (cm)"])  # Encabezado

    while True:
        try:
            linea = ser.readline().decode().strip()  # Lee y limpia la línea
            if linea.startswith("Distancia:"):
                distancia = linea.split(":")[1].strip().split()[0]  # Extrae la distancia
                tiempo = ser.in_waiting  # Simula el tiempo con los bytes en espera
                writer.writerow([tiempo, distancia])
                print(f"Guardado: {tiempo} ms, {distancia} cm")
        except KeyboardInterrupt:
            print("\nFinalizando...")
            break

# Cierra la conexión serial
ser.close()
