package fittrack.interfaz

import javax.swing.JPanel

/** Cada pantalla de la aplicación: un panel y la forma de volver a dibujarse con el estado actual. */
trait Vista {
    def panel: JPanel
    def refrescar(): Unit
}
