module class_Inventory

    use class_Product ! Importamos la clase producto
    use class_Analyzer
    public
    integer :: numProducts

    contains ! Funciones y subrutinas

    ! Subrutina para la lectura de los archivos
    subroutine readFile()

        character (len = 100) :: linea
        
        integer estadoApertura, i

        ! Abrimos el archivo
        OPEN(UNIT=1, FILE='incial.inv', STATUS='OLD', ACTION='READ', IOSTAT= estadoApertura)

        ! Si la apertura del archivo no tiene errores
        if ( estadoApertura /= 0 ) then

            print *, "A ocurrido un error al abrir el archivo"

            stop

        end if
        
        ! Si la apertura del archico es correcta

        i = 0
            
        DO  ! Suponemos que el archivo tiene como máximo 4 líneas

            READ(1, '(A)', IOSTAT=estadoApertura) linea

            if (estadoApertura /= 0) exit

            i = i + 1

            print *, 'Línea ', i, ': ', trim(linea)

            call analizeCadena(linea)
            
        END DO

            ! Cerramos el archivo
        close(1)
  
    end subroutine readFile

    subroutine addProduct(array,  name, amount, price, location)

        type(Product), allocatable, intent(inout) :: array(:)
        type(Product), allocatable :: temp(:) !array temporal
        type(Product) :: newProduct
        character(len=100), intent(in) ::  name, location
        integer, intent(in)  :: amount
        real, intent(in) :: price

        newProduct%name = name
        newProduct%amount = amount
        newProduct%price = price
        newProduct%location = location

        numProducts = numProducts + 1

        print *, numProducts

        if ( allocated(array) ) then

            allocate(temp(numProducts-1))
            temp = array
            deallocate(array)
            allocate(array(numProducts))
            array(1:numProducts-1) = temp
            deallocate(temp)

        else
            allocate(array(numProducts))
        end if
        array(numProducts) = newProduct
        
    end subroutine addProduct

    subroutine addStock(array,  name, amount, location)
        type(Product), allocatable, intent(inout) :: array(:)
        character(len=100), intent(in) ::  name, location
        integer, intent(inout)  :: amount
        integer :: n
        logical :: isOkay
        
        isOkay = .true.

        n = size(array)

        do i = 1, n

            if ( array(i)%name == name .and. array(i)%location == location) then

                amount = array(i)%amount + amount

                call  array(i)%setAmount(amount)

                isOkay = .false.

            end if
            
        end do

        if ( isOkay ) print *, "No se encontro ningun registro del Producto", name, " en ", location

    end subroutine addStock

    subroutine reduceStock(array,  name, amount, location)
        type(Product), allocatable, intent(inout) :: array(:)
        character(len=100), intent(in) ::  name, location
        integer, intent(inout)  :: amount
        integer :: n
        logical :: isOkay
        
        isOkay = .true.

        n = size(array)

        do i = 1, n

            if ( array(i)%name == name .and. array(i)%location == location) then

                if ( array(i)%amount >= amount ) then

                    amount = array(i)%amount + amount

                    call  array(i)%setAmount(amount)

                else

                    print *, "¡A ocurrido un error al actualizar!"
                    print *, "Error: la cantidad a eliminar es mayor a la cantidad en esa ubicación"
                    print *, "Producto: ", array(i)%name, " Stock: ", array(i)%amount, "Reajuste: -", amount
                    
                end if

                isOkay = .false.

            end if
            
        end do

        if ( isOkay ) print *, "No se encontro ningun registro del Producto", name, " en ", location

    end subroutine reduceStock
    
end module class_Inventory