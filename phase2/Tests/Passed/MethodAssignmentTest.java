class A{
    A(){}

    int m(){
        return 0;
    }
}

class B{
    // int x = new A();

    int test(){
        A x = new A();
        return x.m();
    }
}