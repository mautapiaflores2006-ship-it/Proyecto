package fittrack.interfaz

import fittrack.logica.AppEstado
import fittrack.util.*

/** Puente entre las vistas y la lógica pura: aplica cambios al estado y avisa a las vistas. */
class Controlador(
    almacen: Almacen[AppEstado],
    refrescar: () => Unit,
    navegar: String => Unit
) {

    def estado: AppEstado = almacen.obtener

    /** Aplica un cambio que no puede fallar. */
    def cambiar(cambio: AppEstado => AppEstado): Unit = {
        almacen.actualizar(cambio)
        refrescar()
    }

    /** Aplica una operación que puede fallar. Devuelve el mensaje de error, o None si salió bien. */
    def intentar(operacion: AppEstado => Resultado[AppEstado]): Option[String] = {
        operacion(almacen.obtener) match
            case Exito(nuevo) =>
                almacen.actualizar(_ => nuevo)
                refrescar()
                None
            case Fallo(mensaje) => Some(mensaje)
    }

    def irA(pantalla: String): Unit = navegar(pantalla)
}
