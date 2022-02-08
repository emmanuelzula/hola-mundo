program segundo_examen

    real a(1:1000,1:1000),t(0:1000),x(0:1000)
    
    open(1,file='plot.dat',status='replace')
    
    y0=0.
    yn=4.
    n=100
    h=0.02
    
    !Asignación de tn
    do i=0,n
        t(i)=h*(i)
    end do
    
    !Matriz a
    do i=1,n-1

        if(i==1) then
            a(i,i)=h**2.-2.
            a(i,i+1)=1.-h
            cycle
        end if

        if(i==n-1) then
            a(i,i-1)=1.+h
            a(i,i)=h**2.-2.
            cycle
        end if

        a(i,i-1)=1.+h
        a(i,i)=h**2.-2.
        a(i,i+1)=1.-h
    end do 
    
    !Matriz b
    do i=1,n-1
         a(i,n)=(h**2.)*t(i)*(exp(t(i))-1.)
         if(i==1) a(i,n)=-y0*(1.+h)+(h**2.)*t(i)*(exp(t(i))-1.)
         if(i==n-1) a(i,n)=-yn*(1.-h)+(h**2.)*t(i)*(exp(t(i))-1.)
    end do
    
    n=n-1
    
    !Trasnforma la matriz en triangular superior
    do i=1,n
        do j=i+1,n
            a(j,:)=a(j,:)-a(j,i)*a(i,:)/a(i,i)
        end do
    enddo
    
    !Resuelve el sitema de ecuaciones
    x(n)=a(n,n+1)/a(n,n)
    do k=n-1,1,-1
        suma=0
        do j=k+1,n
            suma= suma+x(j)*a(k,j)
        end do
        x(k)= (a(k,n+1)-(suma))/a(k,k)
    end do
    
    !Imprime las soluciones
    x(0)=0.
    x(100)=4.
    
    write(*,*) 'Solucion'
    do k=0,100
    write(*,*) 't',t(k),' = ',x(k),f(t(k))
    write(1,*) t(k),x(k)!,f(t(k))
    end do
    
    end program segundo_examen

    real function f(x)
    f= (1./6.)*exp(x)*(x**3.-10.*x+12.) + 4.*exp(x-2.)*x-x-2.
    end function

