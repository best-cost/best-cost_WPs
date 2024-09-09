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
			m.time_period as "time_period_mort",
			m.sex,
			m.age,
			m.icd10,
			m.obs_value as "mortality_rate_cardio",
			d.obs_value as "deprivation",
			d.time_period as "time_period_depr",
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
				time_period = max(time_period) and
				not obs_value is NULL
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
			left join
				(
				select
					-- two Finish regions were merged for the purpose of the deprivation database
					-- retain only the larger region (other region is island group that is barely visible on EU scale)
					case
						when nuts_id = 'FI19_20' THEN 'FI19'
						else nuts_id
					end as "nuts_id",
					time_period,
					obs_value
				from
					DEPRIVATION
				where
					freq = 'A'
				group by
					nuts_id
				having
					time_period = max(time_period) and
					not obs_value is null
				order by
					time_period desc
				) d on m.nuts_id = d.nuts_id
		) pre on n.nuts_id = pre.nuts_id
where
	(
	n.levl_code = 2 and
	not (n.cntr_code = 'TR' and n.levl_code = 2) and
	not (n.cntr_code = 'UK' and n.levl_code = 2) and
	not (n.cntr_code = 'LV' and n.levl_code = 2) and
	not (n.cntr_code = 'EE' and n.levl_code = 2) and
	not (n.cntr_code = 'IS' and n.levl_code = 2) and
	not (n.cntr_code = 'RS' and n.levl_code = 2) and
	not (n.cntr_code = 'CY' and n.levl_code = 2) and
	not (n.cntr_code = 'LU' and n.levl_code = 2)
	)
	-- national level data (NUTS0) if more detailed data is absent
	or
	(n.cntr_code = 'TR' and n.levl_code = 0)
	or
	(n.cntr_code = 'IS' and n.levl_code = 0)
	or
	(n.cntr_code = 'UK' and n.levl_code = 0)
	or
	(n.cntr_code = 'LV' and n.levl_code = 0)
	or
	(n.cntr_code = 'EE' and n.levl_code = 0)
	or
	(n.cntr_code = 'RS' and n.levl_code = 0)
	or 
	(n.cntr_code = 'CY' and n.levl_code = 0)
	or 
	(n.cntr_code = 'LU' and n.levl_code = 0)
order by
	n.nuts_id
	
			
	
	
	