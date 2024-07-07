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
    if (a.rows != b.rows || a.cols != b.cols) {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    Matrix m=Matrix create_matrix(int a.row, int a.col);
    for(int i=0,i<a.row,i++){
        for(int j=0,j<a.col,j++){
            m.data[i][j]=a.data[i][j]+b.data[i][j];
        }
    }
    return m;
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    // ToDo
    if (a.rows != b.rows || a.cols != b.cols) {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    Matrix m=Matrix create_matrix(int a.row, int a.col);
    for(int i=0,i<a.row,i++){
        for(int j=0,j<a.col,j++){
            m.data[i][j]=a.data[i][j]-b.data[i][j];
        }
    }
    return m;
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    // ToDo
    if (a.cols != b.rows) {
        printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
        return create_matrix(0, 0);
    }
    Matrix m=Matrix create_matrix(int a.row, int b.col);
    for(int i=0,i<a.row,i++){
        for(int j=0,j<b.col,j++){
            m.data.[i][j]=0;
            for(int k=0,k<a.col,x++){
                    m.data[i][j]=m.data[i][j]+a.data[i][k]*b.data[k][j];
            }
        }
    }
    return m;
}

Matrix scale_matrix(Matrix a, double k)
{
    // ToDo
    Matrix m=Matrix create_matrix(int a.row, int a.col);
    for(int i=0,i<a.row,i++){
        for(int j=0,j<a.col,j++){
            m.data[i][j]=k*a.data[i][j];
        }
    }
    return m;
}

Matrix transpose_matrix(Matrix a)
{
    // ToDo
    Matrix m=Matrix create_matrix(int a.col, int a.raw);
    for(int i=0,i<=a.row,i++){
        for(int j=0,j<=a.col,j++){
            m.data[i][j]=a.data[j][i];
        }
    }
    return m;
}

double det_matrix(Matrix a)
{
    // ToDo
    if (a.rows != a.cols) {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    int n = a.rows;
    if (n == 1) {
        return a.data[0][0];
    }
    double det = 0;
    for (int f = 0; f < n; f++) {
        Matrix temp = create_matrix(n - 1, n - 1);
        for (int i = 1; i < n; i++) {
            int subi = 0;
            for (int j = 0; j < n; j++) {
                if (j == f) continue;
                temp.data[i - 1][subi] = a.data[i][j];
                subi++;
            }
        }
        det += (f % 2 == 0 ? 1 : -1) * a.data[0][f] * det_matrix(temp);
    }
    return det;
    return 0;
}

Matrix inv_matrix(Matrix a)
{
    // ToDo
    if (a.rows != a.cols) {
        printf("Error: The matrix must be a square matrix.\n");
        return create_matrix(0, 0);
    }
    int n = a.rows;
    double det = det_matrix(a);
    if (det == 0) {
        printf("Error: The matrix is singular.\n");
        return create_matrix(0, 0);
    }
    Matrix adj = create_matrix(n, n);
    Matrix inv = create_matrix(n, n);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            Matrix temp = create_matrix(n - 1, n - 1);
            for (int k = 0; k < n; k++) {
                if (k == i) continue;
                for (int l = 0; l < n; l++) {
                    if (l == j) continue;
                    temp.data[k < i ? k : k - 1][l < j ? l : l - 1] = a.data[k][l];
                }
            }
            adj.data[j][i] = (i + j) % 2 == 0 ? det_matrix(temp) : -det_matrix(temp);
        }
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            inv.data[i][j] = adj.data[i][j] / det;
        }
    }
    return inv;
    return create_matrix(0, 0);
}

int rank_matrix(Matrix a)
{
    // ToDo
    int m = a.rows;
    int n = a.cols;
    int rank = 0;
    Matrix temp = create_matrix(m, n);
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            temp.data[i][j] = a.data[i][j];
        }
    }
    for (int row = 0; row < m; row++) {
        if (temp.data[row][row] != 0) {
            for (int col = 0; col < m; col++) {
                if (col != row) {
                    double mult = temp.data[col][row] / temp.data[row][row];
                    for (int i = 0; i < n; i++) {
                        temp.data[col][i] -= mult * temp.data[row][i];
                    }
                }
            }
        } else {
            int reduce = 1;
            for (int i = row + 1; i < m; i++) {
                if (temp.data[i][row] != 0) {
                    for (int j = 0; j < n; j++) {
                        double tmp = temp.data[row][j];
                        temp.data[row][j] = temp.data[i][j];
                        temp.data[i][j] = tmp;
                    }
                    reduce = 0;
                    break;
                }
            }
            if (reduce) {
                rank--;
                for (int i = 0; i < m; i++) {
                    temp.data[i][row] = temp.data[i][m - 1];
                }
            }
            row--;
        }
    }
    for (int i = 0; i < m; i++) {
        if (temp.data[i][i] != 0) {
            rank++;
        }
    }
    return rank;
    return 0;
}

double trace_matrix(Matrix a)
{
    if (a.rows != a.cols) {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    double trace = 0;
    for (int i = 0; i < a.rows; i++) {
        trace += a.data[i][i];
    }
    return trace;
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
