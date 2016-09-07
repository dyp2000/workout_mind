#
# part 2
#
SET SESSION sql_mode = '';
SELECT @@SESSION.sql_mode;

replace into `product` (`id`, `current_owner_id`) 
	select `t2`.`product_id` as `id`, `t1`.`participant_id` as `current_owner_id`
	from `assignment` `t1`
	inner join ( 
		select `product_id`, max(`ts`) as `ts_max`
		from `assignment`
		where `action` = 'm'
		group by `product_id`
	) `t2` on (`t1`.`product_id` = `t2`.`product_id` and `t1`.`ts` = `t2`.`ts_max`);

delete from `product`
where (`id`, `current_owner_id`) in (
	select `t3`.`id`, `t3`.`current_owner_id`
	from `assignment` `t1`
	inner join ( 
		select `product_id`, max(`ts`) as `ts_max`
		from `assignment`
		where `action` = 'm'
		group by `product_id`
	) `t2` on (`t1`.`product_id` = `t2`.`product_id` and `t1`.`ts` = `t2`.`ts_max`)
	left join (
		select `id`, `current_owner_id`
		from `product`
	) `t3` on (`t1`.`product_id` = `t3`.`id` and `t1`.`participant_id` <> `t3`.`current_owner_id`)
	where `t3`.`id` is not null);

delete from `product`
where (`id`, `current_owner_id`) not in (
	select `t3`.`id`, `t3`.`current_owner_id`
	from `assignment` `t1`
	inner join ( 
		select `product_id`, max(`ts`) as `ts_max`
		from `assignment`
		where `action` = 'm'
		group by `product_id`
	) `t2` on (`t1`.`product_id` = `t2`.`product_id` and `t1`.`ts` = `t2`.`ts_max`)
	left join (
		select `id`, `current_owner_id`
		from `product`
	) `t3` on (`t1`.`product_id` = `t3`.`id` and `t1`.`participant_id` = `t3`.`current_owner_id`));
