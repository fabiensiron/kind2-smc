KIND2 ?= ../../../bin/kind2

MODEL ?= medical_infusion.lus
INPUT ?= medical_infusion.json

RUNS ?= 1000
STEPS ?= 30
PRECISION ?= 0.05
CONFIDENCE ?= 0.95
SEED ?= 42

SOLVER ?= z3
SOLVER_MODE ?= incremental

SMC_COMMON = \
	--enable SMC \
	--smt_solver $(SOLVER) \
	--smc_steps $(STEPS) \
	--smc_precision $(PRECISION) \
	--smc_solver_mode $(SOLVER_MODE) \
	--smc_input $(INPUT)

.PHONY: all run fixed apmc quick incremental oneshot help

all: run

run: fixed

fixed:
	$(KIND2) \
		$(SMC_COMMON) \
		--smc_estimator fixed \
		--smc_runs $(RUNS) \
		$(MODEL)

apmc:
	$(KIND2) \
		$(SMC_COMMON) \
		--smc_estimator apmc \
		--smc_confidence $(CONFIDENCE) \
		$(MODEL)

quick:
	$(MAKE) fixed RUNS=100 STEPS=10

incremental:
	$(MAKE) fixed SOLVER_MODE=incremental

oneshot:
	$(MAKE) fixed SOLVER_MODE=oneshot

help:
	@echo "Kind 2 SMC medical infusion example"
	@echo
	@echo "Targets:"
	@echo "  make run          Fixed-sample SMC using the defaults"
	@echo "  make fixed        Same as 'run'"
	@echo "  make apmc         APMC estimator"
	@echo "  make quick        Short smoke test (N=100, K=10)"
	@echo "  make incremental  Fixed estimator with incremental SMT"
	@echo "  make oneshot      Fixed estimator with one-shot SMT"
	@echo
	@echo "Main variables:"
	@echo "  KIND2=$(KIND2)"
	@echo "  MODEL=$(MODEL)"
	@echo "  INPUT=$(INPUT)"
	@echo "  RUNS=$(RUNS)"
	@echo "  STEPS=$(STEPS)"
	@echo "  PRECISION=$(PRECISION)"
	@echo "  CONFIDENCE=$(CONFIDENCE)"
	@echo "  SEED=$(SEED)"
	@echo "  SOLVER=$(SOLVER)"
	@echo "  SOLVER_MODE=$(SOLVER_MODE)"
