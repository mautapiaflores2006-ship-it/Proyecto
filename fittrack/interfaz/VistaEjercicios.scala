package fittrack.interfaz

import java.awt.{BorderLayout, Dimension, GridLayout}
import javax.swing.*
import fittrack.logica.*
import fittrack.modelo.*
import fittrack.util.*

/** Catálogo de ejercicios: buscar, filtrar, añadir y eliminar. */
class VistaEjercicios(controlador: Controlador) extends Vista {

    val panel: JPanel = Componentes.lienzo(new BorderLayout())

    private val campoBusqueda = Componentes.campoTexto(16)
    private val filtroGrupo = Componentes.selector[Option[GrupoMuscular]](
        new Opcion[Option[GrupoMuscular]](None, "Todas las zonas") ::
            GrupoMuscular.todos.map(g => new Opcion[Option[GrupoMuscular]](Some(g), GrupoMuscular.nombre(g)))
    )
    private val filtroTipo = Componentes.selector[Option[TipoEjercicio]](
        new Opcion[Option[TipoEjercicio]](None, "Todos los tipos") ::
            TipoEjercicio.todos.map(t => new Opcion[Option[TipoEjercicio]](Some(t), TipoEjercicio.nombre(t)))
    )
    private val lista = new ListaOpciones[Ejercicio](false)
    private val contador = Componentes.etiqueta(" ", Tema.fuentePequena, Tema.tintaSuave)

    private val campoNombre = Componentes.campoTexto(16)
    private val comboGrupo = Componentes.selector[GrupoMuscular](
        GrupoMuscular.todos.map(g => new Opcion[GrupoMuscular](g, GrupoMuscular.nombre(g)))
    )
    private val comboTipo = Componentes.selector[TipoEjercicio](
        TipoEjercicio.todos.map(t => new Opcion[TipoEjercicio](t, TipoEjercicio.nombre(t)))
    )
    private val comboMedicion = Componentes.selector[Medicion](
        Medicion.todas.map(m => new Opcion[Medicion](m, Medicion.nombre(m)))
    )
    private val aviso = new Aviso()
    private val botonAnadir = new BotonPlano("Añadir al catálogo", Primario)
    private val botonEliminar = new BotonPlano("Eliminar el seleccionado", Peligro)

    construir()

    private def construir(): Unit = {
        val encabezado = Componentes.columna()
        encabezado.setBorder(Tema.margen(0, 0, 24, 0))
        encabezado.add(Componentes.izquierda(Componentes.etiqueta("Ejercicios", Tema.fuenteDisplay, Tema.tinta)))
        encabezado.add(Box.createVerticalStrut(6))
        encabezado.add(Componentes.izquierda(Componentes.etiqueta(
            "Todo lo que puedes hacer. Busca, filtra o añade los tuyos.", Tema.fuenteCuerpo, Tema.tintaSuave)))

        // Búsqueda y filtros
        val filaBusqueda = Componentes.lienzo(new BorderLayout(12, 0))
        filaBusqueda.add(Componentes.etiqueta("Buscar", Tema.fuenteNegrita, Tema.tinta), BorderLayout.WEST)
        filaBusqueda.add(campoBusqueda, BorderLayout.CENTER)
        val filaFiltros = Componentes.lienzo(new GridLayout(1, 2, 12, 0))
        filaFiltros.add(filtroGrupo)
        filaFiltros.add(filtroTipo)
        val filtros = Componentes.lienzo(new BorderLayout(0, 10))
        filtros.add(filaBusqueda, BorderLayout.NORTH)
        filtros.add(filaFiltros, BorderLayout.SOUTH)
        filtros.setBorder(Tema.margen(0, 0, 14, 0))

        val pieLista = Componentes.lienzo(new BorderLayout())
        pieLista.setBorder(Tema.margen(14, 0, 0, 0))
        pieLista.add(contador, BorderLayout.WEST)
        pieLista.add(botonEliminar, BorderLayout.EAST)

        val izquierda = Componentes.lienzo(new BorderLayout())
        izquierda.add(filtros, BorderLayout.NORTH)
        izquierda.add(Componentes.desplazable(lista.lista), BorderLayout.CENTER)
        izquierda.add(pieLista, BorderLayout.SOUTH)

        // Formulario
        val derecha = Componentes.lienzo(new BorderLayout())
        derecha.setPreferredSize(new Dimension(340, 0))
        derecha.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 1, 0, 0, Tema.linea),
            Tema.margen(0, 32, 0, 0)
        ))
        val formulario = Componentes.columna()
        formulario.add(Componentes.izquierda(Componentes.seccion("Un ejercicio nuevo")))
        formulario.add(Componentes.izquierda(Componentes.formulario(List(
            ("Nombre", campoNombre),
            ("Zona del cuerpo", comboGrupo),
            ("Tipo", comboTipo),
            ("¿Cómo se mide?", comboMedicion)
        ))))
        formulario.add(Componentes.izquierda(botonAnadir))
        formulario.add(Box.createVerticalStrut(14))
        formulario.add(Componentes.izquierda(aviso))
        derecha.add(formulario, BorderLayout.NORTH)

        panel.setBorder(Tema.margen(40, 52, 40, 52))
        panel.add(encabezado, BorderLayout.NORTH)
        panel.add(izquierda, BorderLayout.CENTER)
        panel.add(derecha, BorderLayout.EAST)

        lista.lista.setFixedCellHeight(62)
        Componentes.alCambiarTexto(campoBusqueda, () => actualizarLista())
        filtroGrupo.addActionListener(_ => actualizarLista())
        filtroTipo.addActionListener(_ => actualizarLista())
        botonAnadir.addActionListener(_ => anadir())
        botonEliminar.addActionListener(_ => eliminar())
    }

    private def textoEjercicio(e: Ejercicio): String = {
        val nombre = e.nombre.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
        s"<html><b>$nombre</b><br>${GrupoMuscular.nombre(e.grupo)} · ${TipoEjercicio.nombre(e.tipo)} · ${Medicion.nombre(e.medicion)}</html>"
    }

    private def actualizarLista(): Unit = {
        val resultado = GestorRutinas.filtrarCatalogo(
            controlador.estado.catalogo,
            campoBusqueda.getText(),
            Componentes.valorOpcional(filtroGrupo),
            Componentes.valorOpcional(filtroTipo)
        )
        lista.mostrar(resultado.map(e => new Opcion(e, textoEjercicio(e))))
        contador.setText(s"${resultado.length} de ${controlador.estado.catalogo.length} ejercicios")
    }

    private def anadir(): Unit = {
        (Componentes.valorSeleccionado(comboGrupo), Componentes.valorSeleccionado(comboTipo), Componentes.valorSeleccionado(comboMedicion)) match
            case (Some(grupo), Some(tipo), Some(medicion)) =>
                controlador.intentar(estado => estado.agregarEjercicio(campoNombre.getText(), grupo, tipo, medicion)) match
                    case None =>
                        campoNombre.setText("")
                        aviso.mostrar("Ejercicio añadido.", false)
                    case Some(error) =>
                        aviso.mostrar(error, true)
            case _ => aviso.mostrar("Elige la zona del cuerpo, el tipo y cómo se mide.", true)
    }

    private def eliminar(): Unit = {
        lista.seleccionado match
            case None => aviso.mostrar("Primero elige un ejercicio de la lista.", true)
            case Some(ejercicio) =>
                if Componentes.confirmar(panel, s"¿Eliminar «${ejercicio.nombre}»? También se quitará de tus rutinas.") then {
                    controlador.cambiar(estado => estado.eliminarEjercicio(ejercicio))
                    aviso.mostrar("Ejercicio eliminado.", false)
                }
    }

    override def refrescar(): Unit = actualizarLista()
}
