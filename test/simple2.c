#include <stdio.h> 
#include <assert.h>
#include <math.h>
#include <stdlib.h>

    extern void __VERIFIER_error(int);

	// inputs
	int inputs[] = {1,2};

	void calculate_output(int);

	int a1 = 10;

 void calculate_output(int input) {
    cf = 1;
    a1 = 20;
    //__VERIFIER_error(1);
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
