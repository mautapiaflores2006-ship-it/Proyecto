package fittrack.interfaz

import java.awt.{BorderLayout, Dimension, GridLayout}
import javax.swing.*
import fittrack.logica.*
import fittrack.modelo.*
import fittrack.util.*

/** Resumen del progreso: cifras, gráfico elegible, marcas personales y destacados. */
class VistaInicio(controlador: Controlador) extends Vista {

    val panel: JPanel = Componentes.lienzo(new BorderLayout())

    private val mensaje = Componentes.etiqueta(" ", Tema.fuenteCuerpo, Tema.tintaSuave)
    private val cifraSesiones = Componentes.etiqueta("0", Tema.fuenteCifra, Tema.tinta)
    private val cifraVolumen = Componentes.etiqueta("0", Tema.fuenteCifra, Tema.tinta)
    private val cifraSeries = Componentes.etiqueta("0", Tema.fuenteCifra, Tema.tinta)
    private val selectorGrafico = Componentes.selector[Option[Ejercicio]](Nil)
    private val zonaGrafico = Componentes.lienzo(new BorderLayout(0, 8))
    private val textoMarcas = Componentes.parrafo()
    private val textoDestacados = Componentes.parrafo()
    private val botonEjemplo = new BotonPlano("Cargar datos de ejemplo", Secundario)

    construir()

    private def bloqueCifra(cifra: JLabel, leyenda: String): JPanel = {
        val p = Componentes.columna()
        p.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(2, 0, 0, 0, Tema.tinta),
            Tema.margen(12, 0, 0, 0)
        ))
        p.add(Componentes.izquierda(cifra))
        p.add(Componentes.izquierda(Componentes.etiqueta(leyenda, Tema.fuentePequena, Tema.tintaSuave)))
        p
    }

    private def construir(): Unit = {
        // Encabezado
        val titulos = Componentes.columna()
        titulos.add(Componentes.izquierda(Componentes.etiqueta("Tu progreso", Tema.fuenteDisplay, Tema.tinta)))
        titulos.add(Box.createVerticalStrut(6))
        titulos.add(Componentes.izquierda(mensaje))
        val norte = Componentes.lienzo(new BorderLayout())
        norte.setBorder(Tema.margen(0, 0, 28, 0))
        norte.add(titulos, BorderLayout.CENTER)
        norte.add(botonEjemplo, BorderLayout.EAST)

        // Cifras (sin tarjetas: solo un trazo oscuro arriba de cada una)
        val cifras = Componentes.lienzo(new GridLayout(1, 3, 40, 0))
        cifras.add(bloqueCifra(cifraSesiones, "sesiones registradas"))
        cifras.add(bloqueCifra(cifraVolumen, "kilos totales levantados"))
        cifras.add(bloqueCifra(cifraSeries, "series completadas"))

        // Gráfico
        val grafico = Componentes.lienzo(new BorderLayout(0, 12))
        grafico.setBorder(Tema.margen(28, 0, 0, 0))
        grafico.add(selectorGrafico, BorderLayout.NORTH)
        grafico.add(zonaGrafico, BorderLayout.CENTER)

        val centro = Componentes.lienzo(new BorderLayout())
        centro.add(cifras, BorderLayout.NORTH)
        centro.add(grafico, BorderLayout.CENTER)

        // Columna derecha
        val derecha = Componentes.columna()
        derecha.setPreferredSize(new Dimension(340, 0))
        derecha.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 1, 0, 0, Tema.linea),
            Tema.margen(0, 32, 0, 0)
        ))
        derecha.add(Componentes.izquierda(Componentes.seccion("Tus marcas")))
        derecha.add(Componentes.izquierda(textoMarcas))
        derecha.add(Box.createVerticalStrut(28))
        derecha.add(Componentes.izquierda(Componentes.seccion("Lo que más trabajas")))
        derecha.add(Componentes.izquierda(textoDestacados))

        panel.setBorder(Tema.margen(40, 52, 40, 52))
        panel.add(norte, BorderLayout.NORTH)
        panel.add(centro, BorderLayout.CENTER)
        panel.add(derecha, BorderLayout.EAST)

        selectorGrafico.addActionListener(_ => dibujarGrafico())
        botonEjemplo.addActionListener(_ => controlador.cambiar(estado => estado.conHistorialDeEjemplo))
    }

    private def textoMensaje(resumen: ResumenEstadistico): String = {
        resumen.tendencia match
            case Some(t) => Tendencia.mensaje(t)
            case None =>
                if resumen.totalSesiones == 0 then "Registra tu primera sesión en «Entrenar» y aquí verás cómo avanzas."
                else "Sigue registrando sesiones para ver cómo evoluciona tu esfuerzo."
    }

    private def textoDeMarcas(estado: AppEstado): String = {
        val records = Estadisticas.recordsPersonales(estado.historial)
        if records.isEmpty then "Cuando guardes tu primera sesión, aquí verás tus mejores marcas."
        else records.map(par => s"${par._1.nombre}:  ${Textos.marca(par._1, par._2)}").mkString("\n")
    }

    private def textoDeDestacados(resumen: ResumenEstadistico): String = {
        val ejercicio = resumen.ejercicioMasEntrenado match
            case Some(par) => s"Ejercicio: ${par._1.nombre}"
            case None => "Ejercicio: todavía sin datos"
        val grupo = resumen.grupoMasTrabajado match
            case Some(par) => s"Zona del cuerpo: ${GrupoMuscular.nombre(par._1)}"
            case None => "Zona del cuerpo: todavía sin datos"
        val mejor = resumen.sesionMayorVolumen match
            case Some(sesion) => s"Mejor sesión: ${sesion.fecha.descripcion}"
            case None => "Mejor sesión: todavía sin datos"
        s"$ejercicio\n$grupo\n$mejor"
    }

    private def opcionesGrafico(estado: AppEstado): List[Opcion[Option[Ejercicio]]] = {
        val entrenados = Listas.sinRepetidos(estado.historial.flatMap(s => s.registros).map(r => r.ejercicio))
        val primera = new Opcion[Option[Ejercicio]](None, "Esfuerzo total de cada sesión")
        val resto = entrenados.map(e => new Opcion[Option[Ejercicio]](Some(e), s"Progreso en ${e.nombre}"))
        primera :: resto
    }

    private def dibujarGrafico(): Unit = {
        val historial = controlador.estado.historial
        val (titulo, puntos) = Componentes.valorSeleccionado(selectorGrafico) match
            case Some(Some(ejercicio)) => Estadisticas.progresoEjercicio(historial, ejercicio)
            case _ => ("Kilos levantados en cada sesión (repeticiones × peso)", Estadisticas.evolucionVolumen(historial))
        zonaGrafico.removeAll()
        zonaGrafico.add(Componentes.etiqueta(titulo, Tema.fuentePequena, Tema.tintaSuave), BorderLayout.NORTH)
        zonaGrafico.add(new PanelGrafico(puntos), BorderLayout.CENTER)
        zonaGrafico.revalidate()
        zonaGrafico.repaint()
    }

    override def refrescar(): Unit = {
        val estado = controlador.estado
        val resumen = Estadisticas.generarResumen(estado.historial)
        cifraSesiones.setText(resumen.totalSesiones.toString)
        cifraVolumen.setText(Formato.numero(resumen.volumenTotal))
        cifraSeries.setText(resumen.seriesTotales.toString)
        mensaje.setText(textoMensaje(resumen))
        botonEjemplo.setVisible(estado.historial.isEmpty)
        textoMarcas.setText(textoDeMarcas(estado))
        textoDestacados.setText(textoDeDestacados(resumen))
        Componentes.llenarSelector(selectorGrafico, opcionesGrafico(estado))
        dibujarGrafico()
    }
}
