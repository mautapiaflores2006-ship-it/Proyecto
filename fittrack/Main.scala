package fittrack

import fittrack.interfaz.VentanaPrincipal
import javax.swing.SwingUtilities

object Main {
    def main(args: Array[String]): Unit =
        SwingUtilities.invokeLater(() => {
            VentanaPrincipal.configurarEstilo()
            new VentanaPrincipal().mostrar()
        })
}
