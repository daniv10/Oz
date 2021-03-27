
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
 % This program finds the number of changes required to make so that the sum of the results of two dice thrown an unknown number of times would be equal %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define MAX(x,y) x > y ? x : y

int main()
{
    int a[] = {4,4,4};
    int b[] = {3};
    int m = sizeof(a)/sizeof(int);
    int n = sizeof(b)/sizeof(int);
    int sum_a = 0;
    int sum_b = 0;
    int count_big[] = {0,0,0,0,0,0};
    int count_small[] = {0,0,0,0,0,0};
    int sol = 0;
    int i,dif;
    
    for(i=0; i<m; i++)
    {
        sum_a += a[i];
    }
    for(i=0; i<n; i++)
    {
        sum_b += b[i];
    }
    if (sum_b == sum_a)
        sol = 0;
    else if ((float) m/n > 6 || (float) n/m > 6)
        sol = -1;
    else
    {
        if (sum_a > sum_b)
        {
            for(i=0; i<m; i++)
                count_big[a[i]-1]++;
            for(i=0; i<n; i++)
               count_small[b[i]-1]++;
        }
        else
        {
            for(i=0; i<m; i++)
                count_small[a[i]-1]++;
            for(i=0; i<n; i++)
               count_big[b[i]-1]++;
        }
        dif = abs(sum_a - sum_b);
        if (count_big[5] + count_small[0] >= ceil((float) dif/5))
            sol += ceil((float) dif/5);
        else
        {
            sol += count_big[5] + count_small[0];
            dif -= (count_big[5] + count_small[0])*5;
            if (count_big[4] + count_small[1] >= ceil((float) dif/4))
                sol += ceil((float) dif/4);
            else
            {
                sol += count_big[4] + count_small[1];
                dif -= (count_big[4] + count_small[1])*4;
                if (count_big[3] + count_small[2] >= ceil((float) dif/3))
                    sol += ceil((float) dif/3);
                else
                {
                    sol += count_big[3] + count_small[2];
                    dif -= (count_big[3] + count_small[2])*3;
                    if (count_big[2] + count_small[3] >= ceil((float) dif/2))
                        sol += ceil((float) dif/2);
                    else
                    {
                        sol += count_big[2] + count_small[3];
                        dif -= (count_big[2] + count_small[3])*2;
                        if (count_big[1] + count_small[4] >= ceil((float) dif/1))
                            sol += ceil((float) dif/1);
                        else
                        {
                            sol += count_big[1] + count_small[4];
                            dif -= (count_big[1] + count_small[4])*1;
                        }
                    }
                }
            }
            
        }
    }
    printf("%d", sol);
    return 0;
}
