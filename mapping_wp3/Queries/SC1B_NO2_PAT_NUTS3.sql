select
	p.nuts_id,
	p.no2_2021_mean,
	case
		when n.cntr_code = 'TR' then NULL  -- no pop grid coverage of Turkey
		when p.no2_2021_mean = 0 then NULL -- overseas regions aren't covered by EEA pollution maps
		else p.no2_2021_pat
	end as no2_2021_pat,
	n.geom,
	n.levl_code,
	n.cntr_code,
	n.nuts_name
from
	pollution as p,
	nuts_2021 as n
where
	p.nuts_id = n.nuts_id and
	n.levl_code = 3