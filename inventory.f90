module class_Inventory

    use class_Product ! Importamos la clase producto
    use class_Analyzer
    public
    integer :: numProducts

    contains ! Funciones y subrutinas

    ! Subrutina para la lectura del archivo de instrucciones
    subroutine readInstructionsFile(products)
        type(Product), dimension(:), allocatable, intent(inout) :: products
        character (len = 200) :: linea
        character (len = 100) :: action, name, location
        integer :: amount, estadoApertura

        ! Abrimos el archivo
        OPEN(UNIT=2, FILE='instrucciones.inv', STATUS='OLD', ACTION='READ', IOSTAT= estadoApertura)

        ! Si la apertura del archivo no tiene errores
        if ( estadoApertura /= 0 ) then

            print *, "A ocurrido un error al abrir el archivo"

            stop

        end if
        
        ! Si la apertura del archico es correcta
            
        DO

            READ(2, '(A)', IOSTAT=estadoApertura) linea

            if (estadoApertura /= 0) exit

            call analizarCadenaInstrucciones(linea, action, name, location, amount)

            if(trim(action) == "agregar_stock") then

                call addStock(products, name, amount, location)
            
            else if(trim(action) == "eliminar_stock") then

                call reduceStock(products, name, amount, location)
            else
                print *, "No se reconoce el comenado: ", action

            end if
            
        END DO

            ! Cerramos el archivo
        close(2)
  
    end subroutine readInstructionsFile

    ! Subrutina para la lectura del archivo de inicio
    subroutine readInitFile(products)
        type(Product), dimension(:), allocatable, intent(inout) :: products
        character (len = 200) :: linea
        character (len = 100) :: action, name, location
        integer :: amount, estadoApertura
        real :: price

        ! Abrimos el archivo
        OPEN(UNIT=1, FILE='incial.inv', STATUS='OLD', ACTION='READ', IOSTAT= estadoApertura)

        ! Si la apertura del archivo no tiene errores
        if ( estadoApertura /= 0 ) then

            print *, "A ocurrido un error al abrir el archivo"

            stop

        end if
        
        ! Si la apertura del archico es correcta
            
        DO

            READ(1, '(A)', IOSTAT=estadoApertura) linea

            if (estadoApertura /= 0) exit

            call analizarCadenaInicial(linea, action, name, location, price, amount)

            if ( trim(action) == "crear_equipo" ) then

                call addProduct(products, name, amount, price, location)

            else 

                print *, "Intruccion no reconocida: ", action

            end if
            
        END DO

            ! Cerramos el archivo
        close(1)
  
    end subroutine readInitFile

    ! Subrutina para el ingreso del producto al array del inventario
    subroutine addProduct(products,  name, amount, price, location)
        type(Product), dimension(:), allocatable, intent(inout) :: products
        type(Product), allocatable :: temp(:) !products temporal
        character(len=100), intent(in) ::  name, location
        integer, intent(in)  :: amount
        real, intent(in) :: price

        numProducts = numProducts + 1

        if ( allocated(products) ) then

            allocate(temp(numProducts-1))
            temp = products
            deallocate(products)
            allocate(products(numProducts))
            products(1:numProducts-1) = temp
            deallocate(temp)

        else
            allocate(products(numProducts))
        end if

        products(numProducts)%name = name
        products(numProducts)%amount = amount
        products(numProducts)%price = price
        products(numProducts)%location = location 
        
    end subroutine addProduct

    ! Subrutina que añade la cantidad al stock del producto
    subroutine addStock(products,  name, amount, location)
        type(Product), allocatable, intent(inout) :: products(:)
        character(len=100), intent(in) ::  name, location
        integer, intent(inout)  :: amount
        integer :: n
        logical :: isOkay
        
        isOkay = .true.

        n = size(products)

        do i = 1, n

            if ( products(i)%name == name .and. products(i)%location == location) then

                amount = products(i)%amount + amount

                call  products(i)%setAmount(amount)

                isOkay = .false.

            end if
            
        end do

        if ( isOkay ) print *, "No se encontro ningun registro del Producto ", trim(name), " en ", trim(location)

    end subroutine addStock

    ! Subrutina para la reduccion del stok del producto
    subroutine reduceStock(products,  name, amount, location)
        type(Product), allocatable, intent(inout) :: products(:)
        character(len=100), intent(in) ::  name, location
        integer, intent(inout)  :: amount
        integer :: n
        logical :: isOkay
        
        isOkay = .true.

        n = size(products)

        do i = 1, n

            if ( products(i)%name == name .and. products(i)%location == location) then

                if ( products(i)%amount >= amount ) then

                    amount = products(i)%amount - amount

                    call  products(i)%setAmount(amount)

                else

                    print *, "A ocurrido un error al actualizar!"
                    print *, "Error: la cantidad a eliminar es mayor a la cantidad en esa ubicacion"
                    print *, "Producto: ", trim(products(i)%name), " Stock: ", products(i)%amount, "Reajuste: -", amount
                    
                end if

                isOkay = .false.

            end if
            
        end do

        if ( isOkay ) print *, "No se encontro ningun registro del Producto", name, " en ", location

    end subroutine reduceStock

    subroutine createReport(products)
        type(Product), dimension(:), allocatable, intent(in) :: products
        integer :: i
        real :: total

        i = SIZE(products)

        ! Abrir el archivo para escribir (crea el archivo si no existe)
        open(unit=10, file="mi_archivo.txt", status="unknown", action="write")

        if ( i > 0 ) then
            ! Escribir algunas líneas en el archivo
            write(10, *) "Informe de Inventario:"
            write(10, *) "Equipo        Cantidad        Precio Unitario         Valor Total         Ubicacion"
            write(10, *) "___________________________________________________________________________________"

            do i = 1, SIZE(products)

                total = products(i)%amount * products(i)%price

                write(10,"(A, T20, I9, T40, F11.2, T55, F11.2, T80, A)") trim(products(i)%name), products(i)%amount, products(i)%price, total, trim(products(i)%location)

                
                print *, "_______________________________"
                print *, "Producto: " , products(i)%name
                print *, " Cantidad: " , products(i)%amount
                print *, " Precio: " , products(i)%price
                print *, " Ubicacion: " , products(i)%location
                
            end do

            print *, "Archivo creado y escrito exitosamente."
        
        else

            print *, "Ningun Producto en el inventario"
        
        end if

        ! Cerrar el archivo
        close(10)
        
    end subroutine createReport
    
end module class_Inventory