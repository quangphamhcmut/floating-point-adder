module fp_adder (
    input logic [8:0] operand_a,
    input logic [8:0] operand_b,
    input logic [0:0] subtract,
    output logic [8:0] result,
    output logic [0:0] overflow,
    output logic [0:0] underflow,
    output logic [0:0] zero_detect
    );
    wire [0:0] sign_a, sign_b;
    wire [3:0] exp_a, exp_b;
    wire [3:0] frac_a, frac_b;
    wire [3:0] final_exp;
    wire [4:0] exp_diff;
    wire [4:0] frac_diff;
    wire [3:0] bigger_frac;
    wire [3:0] smaller_frac;
    wire [14:0] smaller_mantisa;
    wire [15:0] result_mantisa;
    wire [0:0] result_sign;
    wire [3:0] result_exp;
    wire [3:0] result_frac;

    always_comb begin
    	sign_a = operand_a[8];
	sign_b = operand_b[8];
	exp_a = operand_a[7:4];
	exp_b = operand_b[7:4];
	frac_a = operand_a[3:0];
	frac_b = operand_b[3:0];
	result = {result_sign, result_exp, result_frac};
	zero_detect = ~|result_exp & ~|result_frac;
    end

    exp_indentifier exp_comparation (.exp_a, .exp_b, .exp_diff, .final_exp);
    frac_indentifier frac_comparation (.frac_a, .frac_b, .exp_diff, .frac_diff, .bigger_frac, .smaller_frac);
    shifted_smaller_mantisa shift_smaller_mantisa (.smaller_frac, .exp_diff, .smaller_mantisa);
    mantisas_adder mantisas_addition (.subtract(sign_a^sign_b^subtract), .bigger_mantisa({1'b1, bigger_frac, 10'b0000000000}), .smaller_mantisa, .result_mantisa);
    normalizer normalization (.result_mantisa, .final_exp, .overflow, .underflow, .result_frac, .result_exp);
    sign_indentifier sign_indentation (.sign_a, .sign_b, .subtract, .exp_diff, .frac_diff, .result_sign);
endmodule: fp_adder

module sign_indentifier (
    input logic [0:0] sign_a,
    input logic [0:0] sign_b,
    input logic [0:0] subtract,
    input logic [4:0] exp_diff,
    input logic [4:0] frac_diff,
    output logic [0:0] result_sign
    );
    always_comb begin
	if (~|exp_diff)
	    result_sign = (frac_diff[4]) ? subtract^sign_b : sign_a;
	else
	    result_sign = (exp_diff[4]) ? subtract^sign_b : sign_a;
    end
endmodule: sign_indentifier

module exp_indentifier (
    input logic [3:0] exp_a,
    input logic [3:0] exp_b,
    output logic [4:0] exp_diff,
    output logic [3:0] final_exp
    );
    wire [4:0] tmp;
    onebitfa fa0 (.a(exp_a[0]), .b(~exp_b[0]), .ci(1'b1)  , .sum(exp_diff[0]), .co(tmp[0]));
    onebitfa fa1 (.a(exp_a[1]), .b(~exp_b[1]), .ci(tmp[0]), .sum(exp_diff[1]), .co(tmp[1]));
    onebitfa fa2 (.a(exp_a[2]), .b(~exp_b[2]), .ci(tmp[1]), .sum(exp_diff[2]), .co(tmp[2]));
    onebitfa fa3 (.a(exp_a[3]), .b(~exp_b[3]), .ci(tmp[2]), .sum(exp_diff[3]), .co(tmp[3]));
    assign exp_diff[4] = ~tmp[3];
    assign final_exp = (tmp[3]) ? exp_a : exp_b;
endmodule 

module frac_indentifier (
    input logic [3:0] frac_a,
    input logic [3:0] frac_b,
    input logic [4:0] exp_diff,
    output logic [4:0] frac_diff,
    output logic [3:0] bigger_frac,
    output logic [3:0] smaller_frac
    );
    wire [4:0] tmp;
    onebitfa fa0 (.a(frac_a[0]), .b(~frac_b[0]), .ci(1'b1)  , .sum(frac_diff[0]), .co(tmp[0]));
    onebitfa fa1 (.a(frac_a[1]), .b(~frac_b[1]), .ci(tmp[0]), .sum(frac_diff[1]), .co(tmp[1]));
    onebitfa fa2 (.a(frac_a[2]), .b(~frac_b[2]), .ci(tmp[1]), .sum(frac_diff[2]), .co(tmp[2]));
    onebitfa fa3 (.a(frac_a[3]), .b(~frac_b[3]), .ci(tmp[2]), .sum(frac_diff[3]), .co(tmp[3]));
    assign frac_diff[4] = ~tmp[3];
    always_comb begin
	if (~|exp_diff)
	    begin
	    	case(frac_diff[4])
		    1'b0: begin bigger_frac = frac_a; smaller_frac = frac_b; end
		    1'b1: begin bigger_frac = frac_b; smaller_frac = frac_a; end
		endcase
	    end
	else
	    begin
		case(exp_diff[4])
		    1'b0: begin bigger_frac = frac_a; smaller_frac = frac_b; end
		    1'b1: begin bigger_frac = frac_b; smaller_frac = frac_a; end
		endcase
	    end
    end
endmodule: frac_indentifier

module shifted_smaller_mantisa (
    input logic [3:0] smaller_frac,
    input logic [4:0] exp_diff,
    output logic [14:0] smaller_mantisa
    );
    wire [3:0] shift_value;
    always_comb begin
	case (exp_diff[4])
	    1'b0: shift_value = exp_diff[3:0];
	    1'b1: shift_value = {
		~exp_diff[3] ^ ~|exp_diff[2:0],
		~exp_diff[2] ^ ~|exp_diff[1:0],
		~exp_diff[1] ^ ~exp_diff[0],
		exp_diff[0]
		};
	endcase
    	case (shift_value)
	    4'b0000: smaller_mantisa = {1'b1, smaller_frac, 10'b0000000000};
	    4'b0001: smaller_mantisa = {2'b01, smaller_frac, 9'b000000000};
	    4'b0010: smaller_mantisa = {3'b001, smaller_frac, 8'b00000000};
	    4'b0011: smaller_mantisa = {4'b0001, smaller_frac, 7'b0000000};
	    4'b0100: smaller_mantisa = {5'b00001, smaller_frac, 6'b000000};
	    4'b0101: smaller_mantisa = {6'b000001, smaller_frac, 5'b00000};
	    4'b0110: smaller_mantisa = {7'b0000001, smaller_frac, 4'b0000};
	    4'b0111: smaller_mantisa = {8'b00000001, smaller_frac, 3'b000};
	    4'b1000: smaller_mantisa = {9'b000000001, smaller_frac, 2'b00};
	    4'b1001: smaller_mantisa = {10'b0000000001, smaller_frac, 1'b0};
	    4'b1010: smaller_mantisa = {11'b00000000001, smaller_frac};
	    4'b1011: smaller_mantisa = {12'b000000000001, smaller_frac[3:1]};
	    4'b1100: smaller_mantisa = {13'b0000000000001, smaller_frac[3:2]};
	    4'b1101: smaller_mantisa = {14'b00000000000001, smaller_frac[3]};
	    4'b1110: smaller_mantisa = {15'b000000000000001};
	    4'b1111: smaller_mantisa = {15'b000000000000000};
   	endcase
    end
endmodule: shifted_smaller_mantisa

module mantisas_adder (
    input logic [0:0] subtract,
    input logic [14:0] bigger_mantisa,
    input logic [14:0] smaller_mantisa,
    output logic [15:0] result_mantisa
    );
    wire [15:0] tmp;
    onebitfa fa0 (.a(bigger_mantisa[0]), .b(smaller_mantisa[0]^subtract), .ci(subtract), .sum(result_mantisa[0]), .co(tmp[0]));
    onebitfa fa1 (.a(bigger_mantisa[1]), .b(smaller_mantisa[1]^subtract), .ci(tmp[0]), .sum(result_mantisa[1]), .co(tmp[1]));
    onebitfa fa2 (.a(bigger_mantisa[2]), .b(smaller_mantisa[2]^subtract), .ci(tmp[1]), .sum(result_mantisa[2]), .co(tmp[2]));
    onebitfa fa3 (.a(bigger_mantisa[3]), .b(smaller_mantisa[3]^subtract), .ci(tmp[2]), .sum(result_mantisa[3]), .co(tmp[3]));
    onebitfa fa4 (.a(bigger_mantisa[4]), .b(smaller_mantisa[4]^subtract), .ci(tmp[3]), .sum(result_mantisa[4]), .co(tmp[4]));
    onebitfa fa5 (.a(bigger_mantisa[5]), .b(smaller_mantisa[5]^subtract), .ci(tmp[4]), .sum(result_mantisa[5]), .co(tmp[5]));
    onebitfa fa6 (.a(bigger_mantisa[6]), .b(smaller_mantisa[6]^subtract), .ci(tmp[5]), .sum(result_mantisa[6]), .co(tmp[6]));
    onebitfa fa7 (.a(bigger_mantisa[7]), .b(smaller_mantisa[7]^subtract), .ci(tmp[6]), .sum(result_mantisa[7]), .co(tmp[7]));
    onebitfa fa8 (.a(bigger_mantisa[8]), .b(smaller_mantisa[8]^subtract), .ci(tmp[7]), .sum(result_mantisa[8]), .co(tmp[8]));
    onebitfa fa9 (.a(bigger_mantisa[9]), .b(smaller_mantisa[9]^subtract), .ci(tmp[8]), .sum(result_mantisa[9]), .co(tmp[9]));
    onebitfa fa10 (.a(bigger_mantisa[10]), .b(smaller_mantisa[10]^subtract), .ci(tmp[9]), .sum(result_mantisa[10]), .co(tmp[10]));
    onebitfa fa11 (.a(bigger_mantisa[11]), .b(smaller_mantisa[11]^subtract), .ci(tmp[10]), .sum(result_mantisa[11]), .co(tmp[11]));
    onebitfa fa12 (.a(bigger_mantisa[12]), .b(smaller_mantisa[12]^subtract), .ci(tmp[11]), .sum(result_mantisa[12]), .co(tmp[12]));
    onebitfa fa13 (.a(bigger_mantisa[13]), .b(smaller_mantisa[13]^subtract), .ci(tmp[12]), .sum(result_mantisa[13]), .co(tmp[13]));
    onebitfa fa14 (.a(bigger_mantisa[14]), .b(smaller_mantisa[14]^subtract), .ci(tmp[13]), .sum(result_mantisa[14]), .co(tmp[14]));
    assign result_mantisa[15] = tmp[14] ^ subtract;
endmodule: mantisas_adder

module normalizer (
    input logic [15:0] result_mantisa,
    input logic [3:0] final_exp,
    output logic [0:0] overflow,
    output logic [0:0] underflow,
    output logic [3:0] result_frac,
    output logic [3:0] result_exp
    );
    wire [4:0] sel;
    wire [4:0] exp_shift;
    always_comb begin
	sel[0] = ~|result_mantisa[15:1] & result_mantisa[0] |
		 ~|result_mantisa[15:3] & result_mantisa[2] |
		 ~|result_mantisa[15:5] & result_mantisa[4] |
		 ~|result_mantisa[15:7] & result_mantisa[6] |
		 ~|result_mantisa[15:9] & result_mantisa[8] |
		 ~|result_mantisa[15:11] & result_mantisa[10] |
		 ~|result_mantisa[15:13] & result_mantisa[12] |
		 ~result_mantisa[15] & result_mantisa[14];
	sel[1] = ~|result_mantisa[15:2] & result_mantisa[1] |
		 ~|result_mantisa[15:3] & result_mantisa[2] |
		 ~|result_mantisa[15:6] & result_mantisa[5] |
		 ~|result_mantisa[15:7] & result_mantisa[6] |
		 ~|result_mantisa[15:10] & result_mantisa[9] |
		 ~|result_mantisa[15:11] & result_mantisa[10] |
		 ~|result_mantisa[15:14] & result_mantisa[13] |
		 ~result_mantisa[15] & result_mantisa[14];
	sel[2] = ~|result_mantisa[15:4] & result_mantisa[3] |
		 ~|result_mantisa[15:5] & result_mantisa[4] |
		 ~|result_mantisa[15:6] & result_mantisa[5] |
		 ~|result_mantisa[15:7] & result_mantisa[6] |
		 ~|result_mantisa[15:12] & result_mantisa[11] |
		 ~|result_mantisa[15:13] & result_mantisa[12] |
		 ~|result_mantisa[15:14] & result_mantisa[13] |
		 ~result_mantisa[15] & result_mantisa[14];
	sel[3] = ~|result_mantisa[15:8] & result_mantisa[7] |
		 ~|result_mantisa[15:9] & result_mantisa[8] |
		 ~|result_mantisa[15:10] & result_mantisa[9] |
		 ~|result_mantisa[15:11] & result_mantisa[10] |
		 ~|result_mantisa[15:12] & result_mantisa[11] |
		 ~|result_mantisa[15:13] & result_mantisa[12] |
		 ~|result_mantisa[15:14] & result_mantisa[13] |
		 ~result_mantisa[15] & result_mantisa[14];
	sel[4] = result_mantisa[15];
    end
    wire [3:0] frac_tmp;
    always_comb begin
	case (sel)
	    5'b00000: frac_tmp = 4'b0000;
	    5'b00001: frac_tmp = 4'b0000;
	    5'b00010: frac_tmp = {result_mantisa[0], 3'b000};
	    5'b00011: frac_tmp = {result_mantisa[1:0], 2'b00};
	    5'b00100: frac_tmp = {result_mantisa[2:0], 1'b0};
	    5'b00101: frac_tmp = result_mantisa[3:0];
	    5'b00110: frac_tmp = result_mantisa[4:1];
	    5'b00111: frac_tmp = result_mantisa[5:2];
	    5'b01000: frac_tmp = result_mantisa[6:3];
	    5'b01001: frac_tmp = result_mantisa[7:4];
	    5'b01010: frac_tmp = result_mantisa[8:5];
	    5'b01011: frac_tmp = result_mantisa[9:6];
	    5'b01100: frac_tmp = result_mantisa[10:7];
	    5'b01101: frac_tmp = result_mantisa[11:8];
	    5'b01110: frac_tmp = result_mantisa[12:9];
	    5'b01111: frac_tmp = result_mantisa[13:10];
	    5'b10000: frac_tmp = result_mantisa[14:11];
	    default: frac_tmp = result_mantisa[14:11];
	endcase
    end
    always_comb begin
    	case (sel)
	    5'b00000: exp_shift = final_exp;
	    5'b00001: exp_shift = 4'b1110; 
	    5'b00010: exp_shift = 4'b1101;
	    5'b00011: exp_shift = 4'b1100;
	    5'b00100: exp_shift = 4'b1011;
	    5'b00101: exp_shift = 4'b1010;
	    5'b00110: exp_shift = 4'b1001;
	    5'b00111: exp_shift = 4'b1000;
	    5'b01000: exp_shift = 4'b0111;
	    5'b01001: exp_shift = 4'b0110;
	    5'b01010: exp_shift = 4'b0101;
	    5'b01011: exp_shift = 4'b0100;
	    5'b01100: exp_shift = 4'b0011;
	    5'b01101: exp_shift = 4'b0010;
	    5'b01110: exp_shift = 4'b0001;
	    5'b01111: exp_shift = 4'b0000;
	    5'b10000: exp_shift = 4'b1111;
	    default:  exp_shift = 4'b1111;
    	endcase
    end
    wire [4:0] tmp;
    wire [3:0] exp_tmp;
    onebitfa fa0 (.a(final_exp[0]), .b(~exp_shift[0]), .ci(1'b1), .sum(exp_tmp[0]), .co(tmp[0]));
    onebitfa fa1 (.a(final_exp[1]), .b(~exp_shift[1]), .ci(tmp[0]), .sum(exp_tmp[1]), .co(tmp[1]));
    onebitfa fa2 (.a(final_exp[2]), .b(~exp_shift[2]), .ci(tmp[1]), .sum(exp_tmp[2]), .co(tmp[2]));
    onebitfa fa3 (.a(final_exp[3]), .b(~exp_shift[3]), .ci(tmp[2]), .sum(exp_tmp[3]), .co(tmp[3]));
    assign underflow = tmp[3]^(~result_mantisa[15]);
    assign overflow = result_mantisa[15]&(&exp_tmp);

    always_comb begin
	if (&final_exp) begin result_exp = final_exp; result_frac = 4'b0000; end
	else begin
	    result_exp = (overflow) ? final_exp : ((underflow) ? 4'b0000 : exp_tmp);
	    result_frac = (overflow) ? 4'b0000 : ((underflow) ? 4'b0000 : frac_tmp);
	end
    end
endmodule: normalizer

module onebitfa (
    input logic [0:0] a,
    input logic [0:0] b,
    input logic [0:0] ci,
    output logic [0:0] sum,
    output logic [0:0] co
    );
    always_comb begin
    	sum = a ^ b ^ ci;
	co = a&b | ci&(a^b);
    end
endmodule: onebitfa
