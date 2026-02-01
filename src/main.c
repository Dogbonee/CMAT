#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <tice.h>
#include <fatdrvce.h>
#include <fileioc.h>
#include <graphx.h>

#define KEY_MODE  69
#define KEY_LEFT 2
#define KEY_RIGHT 1
#define KEY_UP 3
#define KEY_DOWN 4
#define KEY_ENTER 5
#define KEY_CLEAR 9

#define SCREEN_WIDTH 320
#define SCREEN_HEIGHT 240

#define EPSILON 1e-6f



typedef struct {
    float r; // Real part
    float i; // Imaginary part
} Complex;

// --- Helper Macros/Functions for Complex Math ---

// Magnitude (Absolute value)
float c_abs(Complex a) {
    return sqrtf(a.r * a.r + a.i * a.i);
}

// Division: a / b
Complex c_div(Complex a, Complex b) {
    Complex res;
    float denom = b.r * b.r + b.i * b.i;
    res.r = (a.r * b.r + a.i * b.i) / denom;
    res.i = (a.i * b.r - a.r * b.i) / denom;
    return res;
}

// Multiplication: a * b
Complex c_mul(Complex a, Complex b) {
    Complex res;
    res.r = a.r * b.r - a.i * b.i;
    res.i = a.r * b.i + a.i * b.r;
    return res;
}

// Subtraction: a - b
Complex c_sub(Complex a, Complex b) {
    Complex res;
    res.r = a.r - b.r;
    res.i = a.i - b.i;
    return res;
}

// Multiply by scalar (for pivoting)
Complex c_scale(Complex a, float s) {
    Complex res;
    res.r = a.r * s;
    res.i = a.i * s;
    return res;
}

Complex* complex_rref(int rows, int cols, const Complex *matrix) {

    if (matrix == NULL) {
        return NULL;
    }

    Complex* A = (Complex*)malloc(sizeof(Complex) * rows * cols);

    // Copy the matrix
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            A[i * cols + j] = matrix[i * cols + j];
        }
    }

    int lead = 0;

    for (int r = 0; r < rows; r++) {
        if (cols <= lead) {
            break;
        }

        int i = r;

        // Find pivot
        while (c_abs(A[i * cols + lead]) < EPSILON) {
            i++;
            if (rows == i) {
                i = r;
                lead++;
                if (cols == lead) {
                    return A;
                }
            }
        }

        // Swap rows
        if (i != r) {
            for (int j = 0; j < cols; j++) {
                Complex temp = A[r * cols + j];
                A[r * cols + j] = A[i * cols + j];
                A[i * cols + j] = temp;
            }
        }

        // Normalize row
        Complex div = A[r * cols + lead];
        if (c_abs(div) > EPSILON) {
            for (int j = 0; j < cols; j++) {
                A[r * cols + j] = c_div(A[r * cols + j], div);
            }
        }

        // Eliminate other rows
        for (int k = 0; k < rows; k++) {
            if (k != r) {
                Complex mul = A[k * cols + lead];
                for (int j = 0; j < cols; j++) {
                    Complex term = c_mul(mul, A[r * cols + j]);
                    A[k * cols + j] = c_sub(A[k * cols + j], term);
                }
            }
        }
        lead++;
    }
    return A;
}


void print_matrix(int rows, int cols, Complex *A) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if(fabsf(A[i*cols+j].r) < EPSILON) A[i*cols+j].r = 0.0f;
            if(fabsf(A[i*cols+j].i) < EPSILON) A[i*cols+j].i = 0.0f;

            printf("(%.1f, %.1f) ", A[i * cols + j].r, A[i * cols + j].i);
        }
        printf("\n");
    }
}

Complex* parse_matrix(char matrix[9][9][16], int rows, int columns) {
    Complex* parsedMatrix = (Complex*)malloc(sizeof(Complex) * rows * columns);

    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < columns; col++) {
            float real = 0;
            float imag = 0;
            float container = 0;
            int realPart = 1;
            int negative = 0;
            int decimal = 0;

            for (int i = 0; matrix[row][col][i] != 0; i++) {
                char c = matrix[row][col][i];
                if (c >= 48 && c < 58) {
                    if (decimal) {
                        container = fabs(container) + (c - 48) * pow(10, -decimal);
                        decimal++;
                    } else {
                        container = fabs(container * 10) + (c - 48);
                    }

                    if (negative) {
                        container *= -1;
                    }

                } else if (c == '-') {
                    negative = 1;
                    if (realPart == 1) {
                        real = container;
                    } else {
                        imag = container;
                    }
                    decimal = 0;
                    realPart = 1;
                    container = 0;
                } else if (c == '+') {
                    if (realPart == 1) {
                        real = container;
                    } else {
                        imag = container;
                    }
                    negative = 0;
                    realPart = 1;
                    decimal = 0;
                    container = 0;
                 } else if (c == '.') {
                     decimal = 1;
                 } else if (c == 'i') {
                    realPart = 0;
                     if (!container) {
                         container++;
                     }
                    imag = container;

                    container = 0;
                }
            }

            if (realPart == 1 && container != 0) {
                real = container;
            } else if (container != 0){
                imag = container;
            }

            Complex complex = {real, imag};
            parsedMatrix[row * columns + col] = complex;
        }
    }
    return parsedMatrix;
}



char*** serialize_matrix(Complex* matrix, int rows, int columns) {

    char*** serializedMatrix = (char***)malloc(sizeof(char**) * rows);
    for (int row = 0; row < rows; row++) {
        serializedMatrix[row] = (char**)malloc(sizeof(char*) * columns);
        for (int col = 0; col < columns; col++) {
            serializedMatrix[row][col] = (char*)malloc(sizeof(char) * 32);
            serializedMatrix[row][col][0] = 0;

            char buf[32];
            float real = matrix[row * columns + col].r;
            float imag = matrix[row * columns + col].i;

            if (real != 0) {
                sprintf(buf, "%.1f", real);
                strcat(serializedMatrix[row][col], buf);
            }
            if (imag < 0 || (imag > 0 && real == 0)) {
                sprintf(buf, "%.1fi", imag);
                strcat(serializedMatrix[row][col], buf);
            } else if (imag > 0 && real != 0) {
                sprintf(buf, "+%.1fi", imag);
                strcat(serializedMatrix[row][col], buf);
            }
            if (real == 0 && imag == 0) {
                sprintf(buf, "0");
                strcpy(serializedMatrix[row][col], buf);
            }
        }
    }


    return serializedMatrix;

}

struct Pair{
    int x;
    int y;
} typedef Pair;



void print_rref_ui(int rows, int columns, char*** serializedMatrix, Complex* solvedMatrix, bool inGrid, Pair gridCursor) {
    Pair gridOffset = {20, 30};
    char resultBuf[32] = {};
    const Complex currentResult = solvedMatrix[gridCursor.x * columns + gridCursor.y];
    const float real = currentResult.r;
    const float imag = currentResult.i;
    char buf[32];

    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < columns; col++) {

            if (inGrid && gridCursor.x == row && gridCursor.y == col) {
                gfx_FillRectangle(gridOffset.x + col*(240/columns), gridOffset.y + row*(120/rows),240/columns, 120/rows);
                gfx_SetTextScale(1, 1);
                gfx_SetTextTransparentColor(0);
                gfx_SetTextFGColor(255);
                gfx_SetTextBGColor(0);
                unsigned int textWidth = gfx_GetStringWidth(serializedMatrix[row][col]);
                gfx_PrintStringXY(serializedMatrix[row][col], gridOffset.x + col*(240/columns) + (240/columns) / 2 - textWidth/2, gridOffset.y + row*(120/rows) + (120/rows) / 2);
                gfx_SetTextTransparentColor(255);
                gfx_SetTextFGColor(0);
                gfx_SetTextBGColor(255);
                gfx_SetTextScale(2, 2);
            }
            else {
                gfx_Rectangle(gridOffset.x + col*(240/columns), gridOffset.y + row*(120/rows),240/columns, 120/rows);
                gfx_SetTextScale(1, 1);
                unsigned int textWidth = gfx_GetStringWidth(serializedMatrix[row][col]);
                gfx_PrintStringXY(serializedMatrix[row][col], gridOffset.x + col*(240/columns) + (240/columns) / 2 - textWidth/2, gridOffset.y + row*(120/rows) + (120/rows) / 2);
                gfx_SetTextScale(2, 2);
            }


            if (!inGrid) {
                gfx_SetTextTransparentColor(0);
                gfx_SetTextFGColor(255);
                gfx_SetTextBGColor(0);
                gfx_FillRectangle(20, SCREEN_HEIGHT - 40, SCREEN_WIDTH - 30, 30);
                unsigned int rrefWidth = gfx_GetStringWidth("BACK");
                gfx_PrintStringXY("BACK", SCREEN_WIDTH/2 - rrefWidth/2, SCREEN_HEIGHT - 30);
                gfx_SetTextTransparentColor(255);
                gfx_SetTextFGColor(0);
                gfx_SetTextBGColor(255);
            } else {
                gfx_Rectangle(20, SCREEN_HEIGHT - 40, SCREEN_WIDTH - 30, 30);
                unsigned int rrefWidth = gfx_GetStringWidth("BACK");
                gfx_PrintStringXY("BACK", SCREEN_WIDTH/2 - rrefWidth/2, SCREEN_HEIGHT - 30);
            }

        }
    }


    if (inGrid) {
        if (real != 0) {
            sprintf(buf, "%.4f", real);
            strcat(resultBuf, buf);
        }
        if (imag < 0 || (imag > 0 && real == 0)) {
            sprintf(buf, "%.4fi", imag);
            strcat(resultBuf, buf);
        } else if (imag > 0 && real != 0) {
            sprintf(buf, "+%.4fi", imag);
            strcat(resultBuf, buf);
        }
        if (real == 0 && imag == 0) {
            sprintf(buf, "0");
            strcpy(resultBuf, buf);
        }

        gfx_PrintStringXY(resultBuf, 20, SCREEN_HEIGHT - 80);
    }

    gfx_BlitBuffer();
}

void print_rref_matrix(Complex* matrix, int rows, int columns) {
    gfx_FillScreen(255);
    Complex* solvedMatrix = complex_rref(rows, columns, matrix);

    if (solvedMatrix == NULL) {
        free(matrix);
        return;
    }

    char*** serializedMatrix = serialize_matrix(solvedMatrix, rows, columns);

    bool inGrid = 0;
    Pair gridCursor = {rows - 1,0};
    print_rref_ui(rows, columns, serializedMatrix, solvedMatrix, inGrid, gridCursor);


    uint16_t key = os_GetKey();
    while (key != KEY_ENTER && key != KEY_MODE) {
        gfx_FillScreen(255);
        if (key == KEY_LEFT) {
           if (inGrid && (gridCursor.x != 0 || gridCursor.y != 0)) {
                gridCursor.y--;
                if (gridCursor.y < 0) {
                    gridCursor.y = columns - 1;
                    gridCursor.x--;
                }
            } else if (!inGrid) {
                inGrid = 1;
                gridCursor.x = rows - 1;
                gridCursor.y = columns - 1;
            }
        } else if (key == KEY_RIGHT) {
           if (rows - 1 != gridCursor.x || columns - 1 != gridCursor.y) {
                gridCursor.y++;
                if (gridCursor.y >= columns && (rows * columns != gridCursor.x * gridCursor.y)) {
                    gridCursor.y = 0;
                    gridCursor.x++;
                }
            } else {
                inGrid = 0;
            }
        } else if (key == KEY_DOWN) {
            if (gridCursor.x < rows - 1){
                gridCursor.x++;
                if (gridCursor.x >= rows) {
                    gridCursor.x--;
                }
            } else {
                inGrid = 0;
            }
        } else if (key == KEY_UP) {
            if (!inGrid) {
                inGrid = 1;
            }
            else {
                gridCursor.x--;
                if (gridCursor.x < 0) {
                    gridCursor.x = 0;
                }
            }
        }
        print_rref_ui(rows, columns, serializedMatrix, solvedMatrix, inGrid, gridCursor);
        key = os_GetKey();
    }

    if (key == KEY_MODE) {
        gfx_End();
        exit(0);
    }


    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < columns; col++) {
            free(serializedMatrix[row][col]);
        }
        free(serializedMatrix[row]);
    }
    free(serializedMatrix);
    free(matrix);
    free(solvedMatrix);
}



void print_ui() {
    enum {
        ROWS,
        COLS,
        NONE
    };

    char matrix [9][9][16];


    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 9; j++) {
            strcpy(matrix[i][j], "0");
        }
    }

    int inGrid = 0;
    int rref = 0;
    Pair cursor = {0,0};

    Pair grid = {1, 1};
    Pair gridCursor = {0,0};
    Pair gridOffset = {20, 60};

    int inputPtr = 0;


    int cursorWidth = 16;
    int num = 1;

    Pair startingOffset = {132, 35};

    char msg[32];
    gfx_SetTextScale(2, 2);

    gfx_SetDrawBuffer();
    sprintf(msg, "MATRIX   %dx%d", grid.x, grid.y);
    gfx_PrintStringXY(msg, 20, 20);
    gfx_FillRectangle(startingOffset.x + cursor.x * cursorWidth, startingOffset.y, 15, 5);
    gfx_BlitBuffer();

    uint16_t key = os_GetKey();

    while (key != KEY_MODE) {

        if (key >= 142 && key < 152) {
            num = key - 142;
            if (!inGrid && !rref && num != 0) {
                if (cursor.x == 0) {
                    grid.x = num;
                    cursor.x = 2;
                } else {
                    grid.y = num;
                    inGrid = 1;
                }
            } else if (inGrid){
                matrix[gridCursor.x][gridCursor.y][inputPtr] = num + 48;
                matrix[gridCursor.x][gridCursor.y][inputPtr + 1] = 0;
                inputPtr++;
            }
            sprintf(msg, "MATRIX   %dx%d", grid.x, grid.y);
        }

        if (key == 140 || key == 129) {
            matrix[gridCursor.x][gridCursor.y][inputPtr] = '-';
            matrix[gridCursor.x][gridCursor.y][inputPtr + 1] = 0;
            inputPtr++;
        }
        if (key == 128) {
            matrix[gridCursor.x][gridCursor.y][inputPtr] = '+';
            matrix[gridCursor.x][gridCursor.y][inputPtr + 1] = 0;
            inputPtr++;
        }
        if (key == 141) {
            matrix[gridCursor.x][gridCursor.y][inputPtr] = '.';
            matrix[gridCursor.x][gridCursor.y][inputPtr + 1] = 0;
            inputPtr++;
        }
        if (key == 238) {
            matrix[gridCursor.x][gridCursor.y][inputPtr] = 'i';
            matrix[gridCursor.x][gridCursor.y][inputPtr + 1] = 0;
            inputPtr++;
        }

        if (key == KEY_ENTER) {
            if (rref) {
                Complex* parsedMatrix = parse_matrix(matrix, grid.x, grid.y);
                // Note: print_rref_matrix takes ownership of parsedMatrix and frees it.
                print_rref_matrix(parsedMatrix, grid.x, grid.y);
            } else if (!inGrid) {
                if (cursor.x == 0) {
                    cursor.x = 2;
                } else {
                    inGrid = 1;
                }
            } else if (grid.x - 1 != gridCursor.x || grid.y - 1 != gridCursor.y) {
                    gridCursor.y++;
                    if (gridCursor.y >= grid.y) {
                        gridCursor.y = 0;
                        gridCursor.x++;
                    }
                inputPtr = 0;
            } else {
                inGrid = 0;
                rref = 1;
            }
        } else if (key == KEY_LEFT) {
            if (!inGrid && !rref && cursor.x == 2) {
                cursor.x = 0;
            } else if (inGrid && gridCursor.x == 0 && gridCursor.y == 0) {
                inGrid = 0;
            } else if (inGrid) {
                gridCursor.y--;
                if (gridCursor.y < 0) {
                    gridCursor.y = grid.y - 1;
                    gridCursor.x--;
                }
                inputPtr = 0;
            }
        } else if (key == KEY_RIGHT) {
            if (!inGrid && !rref && cursor.x == 0) {
                cursor.x = 2;
            } else if (!inGrid && !rref) {
                inGrid = 1;
            } else if (grid.x - 1 != gridCursor.x || grid.y - 1 != gridCursor.y) {
                gridCursor.y++;
                if (gridCursor.y >= grid.y && (grid.x * grid.y != gridCursor.x * gridCursor.y)) {
                    gridCursor.y = 0;
                    gridCursor.x++;
                }
                inputPtr = 0;
            } else {
                inGrid = 0;
                rref = 1;
            }
        } else if (key == KEY_DOWN) {
            if (!inGrid && !rref) {
                inGrid = 1;
            } else if (gridCursor.x < grid.x - 1){
                gridCursor.x++;
                if (gridCursor.x >= grid.x) {
                    gridCursor.x--;
                }
                inputPtr = 0;
            } else {
                inGrid = 0;
                rref = 1;
            }
        } else if (key == KEY_UP) {
            if (inGrid) {
                gridCursor.x--;
                if (gridCursor.x < 0) {
                    gridCursor.x = 0;
                    inGrid = 0;
                }
                inputPtr = 0;
            } else if (rref) {
                rref = 0;
                inGrid = 1;
            }
        } else if (key == KEY_CLEAR) {
            if (inGrid) {
                strcpy(matrix[gridCursor.x][gridCursor.y], "0");
            }
        }

        gfx_FillScreen(255);

        for (int row = 0; row < grid.x; row++) {
            for (int col = 0; col < grid.y; col++) {
                if (inGrid && gridCursor.x == row && gridCursor.y == col) {
                    gfx_FillRectangle(gridOffset.x + col*(240/grid.y), gridOffset.y + row*(120/grid.x),240/grid.y, 120/grid.x);
                    gfx_SetTextScale(1, 1);
                    gfx_SetTextTransparentColor(0);
                    gfx_SetTextFGColor(255);
                    gfx_SetTextBGColor(0);
                    unsigned int textWidth = gfx_GetStringWidth(matrix[row][col]);
                    gfx_PrintStringXY(matrix[row][col], gridOffset.x + col*(240/grid.y) + (240/grid.y) / 2 - textWidth/2, gridOffset.y + row*(120/grid.x) + (120/grid.x) / 2);
                    gfx_SetTextTransparentColor(255);
                    gfx_SetTextFGColor(0);
                    gfx_SetTextBGColor(255);
                    gfx_SetTextScale(2, 2);
                }
                else {
                    gfx_Rectangle(gridOffset.x + col*(240/grid.y), gridOffset.y + row*(120/grid.x),240/grid.y, 120/grid.x);
                    gfx_SetTextScale(1, 1);
                    unsigned int textWidth = gfx_GetStringWidth(matrix[row][col]);
                    gfx_PrintStringXY(matrix[row][col], gridOffset.x + col*(240/grid.y) + (240/grid.y) / 2 - textWidth/2, gridOffset.y + row*(120/grid.x) + (120/grid.x) / 2);
                    gfx_SetTextScale(2, 2);
                }
            }
        }

        gfx_PrintStringXY(msg, 20, 20);
        if (!inGrid && !rref) {
            gfx_FillRectangle(startingOffset.x + cursor.x * cursorWidth, startingOffset.y, 15, 5);
        }


        if (rref) {
            gfx_SetTextTransparentColor(0);
            gfx_SetTextFGColor(255);
            gfx_SetTextBGColor(0);
            gfx_FillRectangle(20, SCREEN_HEIGHT - 40, SCREEN_WIDTH - 30, 30);
            unsigned int rrefWidth = gfx_GetStringWidth("RREF");
            gfx_PrintStringXY("RREF", SCREEN_WIDTH/2 - rrefWidth/2, SCREEN_HEIGHT - 30);
            gfx_SetTextTransparentColor(255);
            gfx_SetTextFGColor(0);
            gfx_SetTextBGColor(255);
        } else {
            gfx_Rectangle(20, SCREEN_HEIGHT - 40, SCREEN_WIDTH - 30, 30);
            unsigned int rrefWidth = gfx_GetStringWidth("RREF");
            gfx_PrintStringXY("RREF", SCREEN_WIDTH/2 - rrefWidth/2, SCREEN_HEIGHT - 30);
        }

#ifdef DEBUG
        char keyText[16];
        sprintf(keyText, "%d", key);
        gfx_PrintStringXY(keyText, 270, 20);
#endif

        gfx_BlitBuffer();
        key = os_GetKey();
    }
}


int main() {
    gfx_Begin();
    print_ui();
    gfx_End();

    os_ClrHome();
    return 0;
}