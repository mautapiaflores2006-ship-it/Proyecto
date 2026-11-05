package fittrack.interfaz

import java.util.concurrent.atomic.AtomicReference

/**
 * Mantiene el estado actual de la aplicación en un único punto.
 * El AppEstado guardado sigue siendo inmutable; la referencia se reemplaza completa.
 */
class Almacen[A](inicial: A) {

    private val valor = new AtomicReference[A](inicial)

    def obtener: A = valor.get()

    def actualizar(cambio: A => A): Unit =
        valor.updateAndGet(actual => cambio(actual))
}
