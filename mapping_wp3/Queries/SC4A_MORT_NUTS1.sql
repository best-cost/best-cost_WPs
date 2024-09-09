select
	m.*,
	n.levl_code,
	n.cntr_code,
	n.nuts_name,
	n.geom
from
	nuts_2021 as n
	left join
		(
		select
			time_period,
			sex,
			age,
			obs_value as "mort_dcs",
			freq,
			icd10,
			nuts_id
		from
			MORTALITY
		where
			sex = 'T' and
			age = 'TOTAL' and
			freq = 'A' and
			icd10 = 'I' -- DCS'
		group by
			nuts_id,
			sex,
			age,
			freq,
			icd10
		having
			time_period = max(time_period)
		order by
			time_period desc
		) m on n.nuts_id = m.nuts_id
where
	LEVL_CODE = 1
			
	
	
	