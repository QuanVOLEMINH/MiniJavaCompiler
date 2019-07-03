package Tests.Passed;

class CastTest{
    CastTest(){

    }
    int convertToDouble(int m){
        return m + 5;
    }
}

class B2 {
    B2(){
        CastTest ct = new CastTest();
        double d = (double)ct.convertToDouble(2);
        d = d + 2;
    }
}