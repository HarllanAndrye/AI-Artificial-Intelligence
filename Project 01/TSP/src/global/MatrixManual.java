package global;

public class MatrixManual {

	private int matrix[][];

	public int[][] getMatrix() {
		return this.matrix;
	}

	public MatrixManual(int qtdCities) {
		matrix = new int[qtdCities][qtdCities];

		if (qtdCities == 4) {
			matrix[0][0] = 0;
			matrix[1][1] = 0;
			matrix[2][2] = 0;
			matrix[3][3] = 0;
			matrix[0][1] = 2;
			matrix[0][2] = 1;
			matrix[0][3] = 4;
			matrix[1][0] = 2;
			matrix[1][2] = 3; 
			matrix[1][3] = 2;
			matrix[2][0] = 1;
			matrix[2][1] = 3;
			matrix[2][3] = 5;
			matrix[3][0] = 4;
			matrix[3][1] = 2;
			matrix[3][2] = 5;
		} else if (qtdCities == 5) {
			matrix[0][0] = 0;
			matrix[1][1] = 0;
			matrix[2][2] = 0;
			matrix[3][3] = 0;
			matrix[4][4] = 0;
			matrix[0][1] = 2;
			matrix[0][2] = 1;
			matrix[0][3] = 4;
			matrix[0][4] = 3;
			matrix[1][0] = 2;
			matrix[1][2] = 3; 
			matrix[1][3] = 1;
			matrix[1][4] = 3;
			matrix[2][0] = 1;
			matrix[2][1] = 3;
			matrix[2][3] = 5;
			matrix[2][4] = 2;
			matrix[3][0] = 4;
			matrix[3][1] = 1;
			matrix[3][2] = 5;
			matrix[3][4] = 1;
			matrix[4][0] = 3;
			matrix[4][1] = 3;
			matrix[4][2] = 2;
			matrix[4][3] = 1;
		} else if (qtdCities == 6) {

		} else if (qtdCities == 7) {

		} else if (qtdCities == 8) {

		} else if (qtdCities == 9) {

		} else if (qtdCities == 10) {

		}
	}

}
