#include <stdio.h> 
#include <assert.h>
#include <math.h>
#include <stdlib.h>

    extern void __VERIFIER_error(int);

	// inputs
	int inputs[] = {1,2};

	void calculate_output(int);
    void errorCheck();

	int a1 = 0;

void errorCheck(){
    if( a1==10 ) {
        __VERIFIER_error(1);    
    }
    if ( a1==20 ){
        __VERIFIER_error(2);
    }
    if ( a1==30 ){
        __VERIFIER_error(3);
    }
}

 void calculate_output(int input) {
    if(input == 1){
        a1 += 10;
    }
    if(input == 2){
        a1 -= 10;
    }

    errorCheck();
}

int main()
{
    // main i/o-loop
    while(1)
    {
        // read input
        int input;
        scanf("%d", &input);        
        // operate eca engine
        if((input != 1) && (input != 2))
          return -2;
        calculate_output(input);
    }
}
