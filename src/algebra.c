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
    //特判不同尺寸
    if (a.cols != b.cols || a.rows != b.rows)
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    // 创建解矩阵变量
    Matrix Ans = create_matrix(a.rows, a.cols);
    //定义游标
    int i, j;
    //遍历执行加法运算
    for (i = 0; i < a.rows; i++)
    {
        for (j = 0; j < a.cols; j++)
        {
            Ans.data[i][j] = a.data[i][j] + b.data[i][j];
        }
    }
    return Ans;
}

Matrix sub_matrix(Matrix a, Matrix b)
{
    //特判不同尺寸
    if (a.cols != b.cols || a.rows != b.rows)
    {
        printf("Error: Matrix a and b must have the same rows and cols.\n");
        return create_matrix(0, 0);
    }
    // 创建解矩阵变量
    Matrix Ans = create_matrix(a.rows, a.cols);
    //定义游标
    int i, j;
    //遍历执行减法运算
    for (i = 0; i < a.rows; i++)
    {
        for (j = 0; j < a.cols; j++)
        {
            Ans.data[i][j] = a.data[i][j] - b.data[i][j];
        }
    }
    return Ans;
}

Matrix mul_matrix(Matrix a, Matrix b)
{
    //特判a列数不等于b行数
    if (a.cols != b.rows)
    {
        printf("Error: The number of cols of matrix a must be equal to the number of rows of matrix b.\n");
        return create_matrix(0, 0);
    }
    // 创建解矩阵变量
    Matrix Ans = create_matrix(a.rows, a.cols);
    //定义游标
    int i, j, k;
    //遍历执行乘法运算
    for (i = 0; i < a.rows; i++)
    {
        for (j = 0; j < b.cols; j++)
        {
            for (k = 0; k < a.cols; k++)
            {
                Ans.data[i][j] += a.data[i][k] * b.data[k][j];
            }
        }
    }
    return Ans;
}

Matrix scale_matrix(Matrix a, double k)
{
    //定义游标
    int i, j;
    //遍历执行数乘运算
    for (i = 0; i < a.rows; i++)
    {
        for (j = 0; j < a.cols; j++)
        {
            a.data[i][j] = a.data[i][j] * k;
        }
    }
    return a;
}

Matrix transpose_matrix(Matrix a)
{
    // 创建解矩阵变量
    Matrix Ans = create_matrix(a.cols, a.rows);
    //定义游标
    int i, j;
    //遍历执行转置运算
    for (i = 0; i < a.cols; i++)
    {
        for (j = 0; j < a.rows; j++)
        {
            Ans.data[i][j] = a.data[j][i];
        }
    }
    return Ans;
}

//执行拉普拉斯变换中去除选定元素行与列的操作
Matrix delete_matrix(Matrix x, int rows, int cols)
{
    Matrix Ans = create_matrix(x.rows - 1, x.cols - 1);
    int i, j;
    for (i = 0; i < x.rows; i++)
    {
        if (i == rows)
        {
            continue;
        }
        for (j = 0; j < x.cols; j++)
        {
            if (j == cols)
            {
                continue;
            }
            else
            {
                Ans.data[i - (i / rows)][j - (j / cols)] = x.data[i][j];
            }
        }
    }
}

double det_matrix(Matrix a)
{
    //特判不是方阵的错误情况
    if (a.rows != a.cols)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    //特判递归时只剩下一个元素的情况，设置递归终点
    if (a.rows == 1)
    {
        return a.data[0][0];
    }
    //定义游标与答案
    int i, j;
    int ans = 0;
    int Temp = 0;
    //遍历并且利用拉普拉斯定理计算矩阵的行列式
    for (i = 0; i < a.cols; i++)
    {
        //获取剩余行列式
        Matrix x = delete_matrix(a, 0, i);
        Temp = a.data[0][i] * det_matrix(x);
        if ((i % 2) == 1)
        {
            Temp = -1 * Temp;
        }
        ans += Temp;
    }
    return ans;
}


Matrix inv_matrix(Matrix a)
{
    //特判矩阵非方阵或行列式为0
    if (a.rows != a.cols || det_matrix(a) == 0)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return create_matrix(0, 0);
    }
    //定义答案矩阵与游标变量
    Matrix Ans = create_matrix(a.rows, a.cols);
    int i, j, det;
    //提前计算矩阵的行列式
    det = det_matrix(a);
    //进行求逆操作
    for (i = 0; i < a.rows; i++)
    {
        for (j = 0; j < a.cols; j++)
        {
            Ans.data[i][j] = det_matrix(delete_matrix(a, i, j)) / det;
            if ((i + j) % 2 == 1)
            {
                Ans.data[i][j] = -1 * Ans.data[i][j];
            }
        }
    }
    return Ans;
}

int rank_matrix(Matrix a)
{
    int i, j, k, Ans, line;
    double Temp;

    //a的秩为行数与列数中较小者，为Ans赋初值
    if (a.rows > a.cols)
    {
        Ans = a.cols;
    }
    else
    {
        Ans = a.rows;
    }

    //初始化行游标
    line = 0;
    for (i = 0; i < a.cols; i++)
    {
        //特判line行i列处为0
        if (a.data[line][i] == 0)
        {
            //寻找到第一个i列位置处不为0的行，进行交换
            for (j = line; j < a.rows; j++);

            //特判i列在line行后都无非0元素的情况
            if (j == a.rows - 1)
            {
                Ans--;
                continue;
            }

            //正常情况交换第一个i列处不为0行与line行
            for (k = 0; k < a.cols; k++)
            {
                Temp = a.data[line][k];
                a.data[line][k] = a.data[j][k];
                a.data[j][k] = Temp;
            }
        }
        //正常执行高斯消元
        for (j = line + 1; j < a.rows; j++)
        {
            Temp = a.data[line][i] / a.data[j][i];
            for (k = 0; k < a.cols; k++)
            {
                a.data[j][k] = a.data[j][k] * Temp - a.data[line][k];
            }
        }
        line++;
    }

    //查询最后一个不全为零的行
    for (i = 0; i < a.rows; i++)
    {
        for (j = 0; j < a.cols; j++)
        {
            if (a.data[i][j] != 0)
            {
                break;
            }
        }
        if (j == a.cols - 1)
        {
            break;
        }
    }
    Ans = i;
    return Ans;
}

double trace_matrix(Matrix a)
{
    //特判非方阵情形
    if (a.rows != a.cols)
    {
        printf("Error: The matrix must be a square matrix.\n");
        return 0;
    }
    //计算矩阵的迹
    int i;
    int Ans = 0;
    for (i = 0; i < a.rows; i++)
    {
        Ans += a.data[i][i];
    }
    return Ans;
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