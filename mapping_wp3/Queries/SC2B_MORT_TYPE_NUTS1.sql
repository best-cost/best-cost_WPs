select
	m.time_period,
	m.sex,
	m.age,
	sum(
	case
		when m.icd10 = 'C' then m.obs_value
		else 0
	end) as mortality_rate_cancer,
	sum(
	case
		when m.icd10 = 'I' then m.obs_value
		else 0
	end) as mortality_rate_cardiovascular,
	sum(
	case
		when not m.icd10 in ('I', 'C') then m.obs_value
		else 0
	end) as mortality_rate_other,
	sum(m.obs_value) as mortality_rate_total,
	n.nuts_id,
	n.levl_code,
	n.cntr_code,
	n.nuts_name,
	n.geom
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
	length(m.icd10) = 1
group by
	m.time_period,
	m.sex,
	m.age,
	n.nuts_id,
	n.levl_code,
	n.cntr_code,
	n.nuts_name,
	n.geom