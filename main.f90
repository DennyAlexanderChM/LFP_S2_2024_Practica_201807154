program main
    use class_Product
    use class_Inventory
    use class_Analyzer

    implicit none

    ! Define variables
    type(Product), dimension(:), allocatable :: products ! Almacenara cada uno de los productos del inventario
    integer:: optionSelect

    !ciclo hasta que la opción seleccionada sea 2 (finalizar programa)
    do while (optionSelect /= 4)
        ! Impresión del menú en pantalla
        print *, "-------------------------------------------------"
        print *, "Practica 1 - Lenguajes formales y de programacion"
        print *, "-------------------------------------------------"
        print *, "# Sistema de inventario"
        print *, " 1. Cargar Inventario Inicial"
        print *, " 2. Cargar Instrucciones de movimientos"
        print *, " 3. Crear Informe de inventario"
        print *, " 4. Salir"
        print *, "Ingrese una opcion:"

        ! Lectura de la opcion desde el teclado
        read(*,*) optionSelect
        ! Select para dirigir a la opción ingresada
        SELECT CASE (optionSelect)
        CASE (1)
            call readInitFile(products)
        CASE (2)
            call readInstructionsFile(products)
        CASE (3)
            call createReport(products)
        CASE (4)
            WRITE(*,*)  'Finalizando...'
            ! Mansaje si se ingresa una opción
        CASE DEFAULT
            WRITE(*,*)  "Ingrese una opción correcta"
        END SELECT
    end do
    
end program main