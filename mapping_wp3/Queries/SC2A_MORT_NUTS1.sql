select
	m.time_period,
	m.sex,
	m.age,
	m.icd10,
	m.nuts_id,
	m.obs_value as mortality_rate_cancer,
	n.geom,
	n.nuts_id,
	n.levl_code,
	n.cntr_code,
	n.nuts_name
from
	mortality as m,
	nuts_2021 as n
where
	m.nuts_id = n.nuts_id and
	m.time_period = 2021 and
	n.levl_code = 1 and
	m.sex = 'T' and
	m.age = 'TOTAL' and
	m.freq = 'A' and
	m.icd10 = 'C'
	