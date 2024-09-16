select
	pre.*,
	n.nuts_id,
	n.levl_code,
	n.cntr_code,
	n.nuts_name,
	n.geom
from
	nuts_2021 as n
	left join
		(
		select 
			m.time_period as "TIME_PERIOD_MORT",
			m.sex,
			m.age,
			m.icd10,
			m.obs_value as "mortality_rate_cardio",
			p.no2_2021_pat,
			p.nuts_id
		from
			(
			select
				time_period,
				sex,
				age,
				obs_value,
				freq,
				icd10,
				nuts_id
			from
				mortality
			where
				sex = 'T' and
				age = 'TOTAL' and
				freq = 'A' and
				icd10 = 'I' -- diseases of circulatory system
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
			) m
			left join
			(	
			select
				no2_2021_pat * 100 as "no2_2021_pat",
				nuts_id
			from
				pollution
			) p on m.nuts_id = p.nuts_id
		) pre on n.nuts_id = pre.nuts_id
where
	n.levl_code = 2
			
	
	
	