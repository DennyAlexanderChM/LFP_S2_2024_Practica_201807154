module class_Analyzer

    use class_Product ! Importamos la clase producto
    public
contains

    subroutine analizeCadena(cadena)
        character(len=100), intent(in) :: cadena
        integer :: i, estado
        character :: actual
        character(len=:), allocatable :: lexema
        character(len=101) :: cadenaNueva

        i = 0

        estado = 1
        
        actual = ""

        lexema = ""

        cadenaNueva = TRIM(cadena) // "#"

        do i = 1, LEN(cadenaNueva) !Quita los espacios y obtiene la dimensian del array
            actual = cadena(i:i)
            
            if ( estado == 1  ) then
                if ( actual /= " " ) then

                    lexema = lexema // actual

                else

                    print *, lexema

                    lexema = ""

                    estado = 2
                    
                end if
            else if(estado == 2) then

                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else
                    estado = 3
                    
                    print *, lexema

                    lexema = ""
                end if
            else if(estado == 3) then
                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else
                    estado = 4
                    print *, lexema
                    lexema = ""
                end if
            else if(estado == 4) then
                if (actual /= ";") then

                    lexema = lexema // actual
                    
                else
                    estado = 5
                    print *, lexema
                    lexema = ""
                end if
            else if(estado == 5) then

                if (actual /= "#") then

                    lexema = lexema // actual
                    
                else
                    print *, lexema, " lexema final"
                end if
            
            end if
                
        end do

    
    end subroutine analizeCadena

end module class_Analyzer