package fittrack.interfaz

import java.awt.{BorderLayout, CardLayout, Component, Dimension}
import javax.swing.*
import javax.swing.plaf.metal.{DefaultMetalTheme, MetalLookAndFeel}
import fittrack.logica.AppEstado

/** Ventana con el menú lateral y las cuatro pantallas. */
class VentanaPrincipal {

    private val cartas = new CardLayout()
    private val contenido = new JPanel(cartas)
    private val grupoBotones = new ButtonGroup()

    private val controlador = new Controlador(
        new Almacen[AppEstado](AppEstado.inicial),
        () => refrescarTodo(),
        pantalla => irA(pantalla)
    )

    private val vistas: List[(String, String, Vista)] = List(
        ("inicio", "Mi progreso", new VistaInicio(controlador)),
        ("ejercicios", "Ejercicios", new VistaEjercicios(controlador)),
        ("rutinas", "Rutinas", new VistaRutinas(controlador)),
        ("sesiones", "Entrenar", new VistaSesiones(controlador))
    )

    private val botones: List[BotonNavegacion] =
        vistas.map(v => crearBoton(v._1, v._2))

    private def crearBoton(clave: String, texto: String): BotonNavegacion = {
        val boton = new BotonNavegacion(texto)
        boton.setActionCommand(clave)
        boton.addActionListener(_ => irA(clave))
        grupoBotones.add(boton)
        boton
    }

    private def refrescarTodo(): Unit =
        vistas.foreach(v => v._3.refrescar())

    private def irA(pantalla: String): Unit = {
        cartas.show(contenido, pantalla)
        botones.filter(b => b.getActionCommand() == pantalla).foreach(b => b.setSelected(true))
    }

    private def construirLateral(): JPanel = {
        val lateral = new JPanel(new BorderLayout())
        lateral.setBackground(Tema.lateral)
        lateral.setPreferredSize(new Dimension(250, 0))

        val marca = Componentes.columna()
        marca.setBackground(Tema.lateral)
        marca.setBorder(Tema.margen(40, 30, 36, 20))
        marca.add(Componentes.izquierda(Componentes.etiqueta("FitTrack", Tema.fuenteDisplay, Tema.lateralTexto)))
        marca.add(Box.createVerticalStrut(4))
        marca.add(Componentes.izquierda(Componentes.etiqueta("CUADERNO · REGISTRO PERSONAL", Tema.fuentePequena, Tema.lateralApagado)))

        val navegacion = Componentes.columna()
        navegacion.setBackground(Tema.lateral)
        botones.foreach(b => {
            b.setAlignmentX(Component.LEFT_ALIGNMENT)
            b.setMaximumSize(new Dimension(Int.MaxValue, 56))
            navegacion.add(b)
        })

        val pie = Componentes.etiqueta("<html>Tus datos se guardan solo<br>mientras la app está abierta.</html>", Tema.fuentePequena, Tema.lateralApagado)
        pie.setBorder(Tema.margen(20, 30, 28, 20))

        lateral.add(marca, BorderLayout.NORTH)
        lateral.add(navegacion, BorderLayout.CENTER)
        lateral.add(pie, BorderLayout.SOUTH)
        lateral
    }

    def mostrar(): Unit = {
        vistas.foreach(v => contenido.add(v._3.panel, v._1))
        contenido.setBackground(Tema.fondo)

        val ventana = new JFrame("FitTrack — Cuaderno de entrenamiento")
        ventana.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
        ventana.getContentPane().setBackground(Tema.fondo)
        ventana.setLayout(new BorderLayout())
        ventana.add(construirLateral(), BorderLayout.WEST)
        ventana.add(contenido, BorderLayout.CENTER)
        ventana.setSize(1240, 800)
        ventana.setMinimumSize(new Dimension(1040, 680))
        ventana.setLocationRelativeTo(null)

        refrescarTodo()
        irA("inicio")
        ventana.setVisible(true)
    }
}

object VentanaPrincipal {

    /** Apariencia plana: tema Metal clásico (sin degradados) y fuentes/colores de la app. */
    def configurarEstilo(): Unit = {
        try {
            MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme())
            UIManager.setLookAndFeel(new MetalLookAndFeel())
        } catch {
            case _: Exception => ()
        }
        UIManager.put("Panel.background", Tema.fondo)
        UIManager.put("OptionPane.background", Tema.fondo)
        UIManager.put("OptionPane.messageFont", Tema.fuenteCuerpo)
        UIManager.put("OptionPane.buttonFont", Tema.fuenteNegrita)
        UIManager.put("OptionPane.messageForeground", Tema.tinta)
        UIManager.put("ComboBox.font", Tema.fuenteCuerpo)
        UIManager.put("ComboBox.background", Tema.superficie)
        UIManager.put("ComboBox.selectionBackground", Tema.acentoSuave)
        UIManager.put("ComboBox.selectionForeground", Tema.tinta)
    }
}
