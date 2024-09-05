select
	case
		when n.cntr_code != 'TR' THEN p.qsic_2021_pwp -- no pop grid coverage of Turkey
		else null
	end as 'qsic_2021_pwp',
	n.nuts_id as "nuts_id",
	n.cntr_code,
	n.nuts_name,
	n.geom
from
	nuts_2021 as n
	left join
		(	
		select
			qsic_2021_pwp,
			nuts_id
		from
			pollution
		) p on n.nuts_id = p.nuts_id
where
	n.levl_code = 3
			
	
	
	