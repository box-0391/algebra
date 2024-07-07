#include "algebra.h"
#include <stdio.h>
#include <math.h>

Matrix create_matrix(int row, int col)
{
    Matrix m;
    m.rows = row;
    m.cols = col;
    return m;
}

Matrix add_matrix(Matrix a, Matrix b)
{
    // ToDo
    Matrix m;
    for(int i=0,i<=a.row,i++){
        for(int j=0,j<=a.col,j++){
            m.data[i][j]=a.data[i][j]+b.data[i][j];
        }
    }
    return m;
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    // ToDo
        Matrix m;
    for(int i=0,i<=a.row,i++){
        for(int j=0,j<=a.col,j++){
            m.data[i][j]=a.data[i][j]-b.data[i][j];
        }
    }
    return m;
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    // ToDo
    Matrix m;
    for(int i=0,i<=a.row,i++){
        for(int j=0,j<=b.col,j++){
            m.data.[i][j]=0;
            for(int x=0,x<=a.col,x++){
                for(int y=0,y<=b.cow,y++){
                    m.data[i][j]=m.data[i][j]+a.data[i][x]*b.data[y][j];
                }
            }
        }
    }
    return m;
}

Matrix scale_matrix(Matrix a, double k)
{
    // ToDo
    return create_matrix(0, 0);
}

Matrix transpose_matrix(Matrix a)
{
    // ToDo
    return create_matrix(0, 0);
}

double det_matrix(Matrix a)
{
    // ToDo
    return 0;
}

Matrix inv_matrix(Matrix a)
{
    // ToDo
    return create_matrix(0, 0);
}

int rank_matrix(Matrix a)
{
    // ToDo
    return 0;
}

double trace_matrix(Matrix a)
{
    // ToDo
    return 0;
}

void print_matrix(Matrix a)
{
    for (int i = 0; i < a.rows; i++)
    {
        for (int j = 0; j < a.cols; j++)
        {
            // 按行打印，每个元素占8个字符的宽度，小数点后保留2位，左对齐
            printf("%-8.2f", a.data[i][j]);
        }
        printf("\n");
    }
}
