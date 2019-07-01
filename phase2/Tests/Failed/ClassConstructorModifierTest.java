package Tests;

public class A extends B {
    String name;

    A(String name) {
        this.name = name;
    }

    public A(int value) {
    }

    native A(){}

    private int a(String l[]) {
        return 3;
    }

}

abstract class B {
}