package fittrack.interfaz

import java.awt.{Dimension, Graphics, Graphics2D, RenderingHints}
import javax.swing.JPanel
import fittrack.modelo.Fecha
import fittrack.util.*

/** Gráfico de barras planas (un color sólido, sin degradados): una barra por sesión. */
class PanelGrafico(puntos: List[(Fecha, Double)]) extends JPanel {

    setOpaque(false)
    setPreferredSize(new Dimension(600, 300))

    private val visibles: List[(Fecha, Double)] = puntos.takeRight(10)

    override protected def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        val g2 = g.asInstanceOf[Graphics2D]
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
        if visibles.isEmpty then dibujarVacio(g2) else dibujarBarras(g2)
    }

    private def centrarTexto(g2: Graphics2D, texto: String, centroX: Int, y: Int): Unit = {
        val ancho = g2.getFontMetrics().stringWidth(texto)
        g2.drawString(texto, centroX - ancho / 2, y)
    }

    private def dibujarVacio(g2: Graphics2D): Unit = {
        g2.setColor(Tema.tintaSuave)
        g2.setFont(Tema.fuenteCuerpo)
        centrarTexto(g2, "Aún no hay datos. Registra una sesión en «Entrenar».", getWidth() / 2, getHeight() / 2)
    }

    private def dibujarBarras(g2: Graphics2D): Unit = {
        val margenInferior = 34
        val margenSuperior = 30
        val hueco = 18
        val ancho = getWidth()
        val alto = getHeight()
        val base = alto - margenInferior
        val altoUtil = alto - margenInferior - margenSuperior

        val maximo = Listas.maximoPor(visibles)(p => p._2) match
            case Some(p) => p._2
            case None => 1.0
        val escala = if maximo <= 0.0 then 1.0 else maximo

        val libre = (ancho - hueco * (visibles.length + 1)) / visibles.length
        val anchoBarra = Math.max(10, Math.min(72, libre))

        g2.setColor(Tema.linea)
        g2.drawLine(0, base, ancho, base)

        def barra(restantes: List[(Fecha, Double)], x: Int): Unit = {
            restantes match
                case Nil => ()
                case (fecha, valor) :: cola =>
                    val altoBarra = Math.max(2, ((valor / escala) * altoUtil).toInt)
                    val y = base - altoBarra
                    val centro = x + anchoBarra / 2
                    g2.setColor(if cola.isEmpty then Tema.acento else Tema.acentoMedio)
                    g2.fillRect(x, y, anchoBarra, altoBarra)
                    g2.setFont(Tema.fuentePequena)
                    g2.setColor(Tema.tinta)
                    centrarTexto(g2, Formato.numero(valor), centro, y - 8)
                    g2.setColor(Tema.tintaSuave)
                    centrarTexto(g2, fecha.corta, centro, alto - 12)
                    barra(cola, x + anchoBarra + hueco)
        }

        barra(visibles, hueco)
    }
}
