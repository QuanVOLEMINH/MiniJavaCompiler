class A{
    A(){}
}

class B{
    // int x = new A();
    B(){}
    int test(){
        int x = new A();
        return 0;
    }
}