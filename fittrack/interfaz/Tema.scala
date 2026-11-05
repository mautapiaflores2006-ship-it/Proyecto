package fittrack.interfaz

import java.awt.{Color, Font, GraphicsEnvironment}
import javax.swing.BorderFactory
import javax.swing.border.Border

/**
 * Identidad visual: cuaderno de anotaciones con papel marfil, tinta azul y margen terracota.
 * Se conserva la jerarquía tipográfica y la distribución de la interfaz original.
 */
object Tema {

    // ===== Colores =====
    // Papel marfil, tinta azul-negra y acentos de cuaderno.
    val fondo = new Color(0xFFFDF5)
    val superficie = new Color(0xF7F3E8)
    val tinta = new Color(0x263746)
    val tintaSuave = new Color(0x66727A)
    val linea = new Color(0xD8DDE0)
    val acento = new Color(0x426B78)         // tinta azul de anotación
    val acentoOscuro = new Color(0x315460)
    val acentoSuave = new Color(0xE1EBEC)
    val acentoMedio = new Color(0xA9C1C5)
    val alerta = new Color(0xB65B54)         // margen rojo / correcciones
    val alertaSuave = new Color(0xF5E5E1)
    val lateral = new Color(0x34444B)        // tapa del cuaderno
    val lateralTexto = new Color(0xFFFDF5)
    val lateralApagado = new Color(0xC2CDD0)

    // ===== Tipografía =====
    private val disponibles: List[String] =
        GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames().toList

    private def primeraDisponible(opciones: List[String], porDefecto: String): String = {
        opciones.filter(o => disponibles.contains(o)) match
            case primera :: _ => primera
            case Nil => porDefecto
    }

    val familiaTitulo: String =
        primeraDisponible(List("Georgia", "Noto Serif", "Liberation Serif", "DejaVu Serif"), Font.SERIF)

    val familiaCuerpo: String =
        primeraDisponible(List("Segoe UI", "Helvetica Neue", "Noto Sans", "Liberation Sans", "DejaVu Sans"), Font.SANS_SERIF)

    val fuenteDisplay = new Font(familiaTitulo, Font.BOLD, 38)
    val fuenteCifra = new Font(familiaTitulo, Font.BOLD, 44)
    val fuenteTitulo = new Font(familiaTitulo, Font.BOLD, 24)
    val fuenteCuerpo = new Font(familiaCuerpo, Font.PLAIN, 16)
    val fuenteNegrita = new Font(familiaCuerpo, Font.BOLD, 16)
    val fuentePequena = new Font(familiaCuerpo, Font.PLAIN, 13)

    // ===== Bordes =====
    def margen(arriba: Int, izquierda: Int, abajo: Int, derecha: Int): Border =
        BorderFactory.createEmptyBorder(arriba, izquierda, abajo, derecha)

    def bordeCampo: Border =
        BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(linea, 1), margen(8, 12, 8, 12))
}
