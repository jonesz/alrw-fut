all: check

check: check_pkde check_nested check_calibrate_predict

check_nested: nested.fut
	futhark check $^

check_pkde: pkde.fut
	futhark check $^

check_calibrate_predict: calibrate_predict.fut
	futhark check $^

.PHONY: clean bench

clean:
	$(RM) example/*.c example/oracle_lei_wasserman
	$(RM) *.c pkde_bench
