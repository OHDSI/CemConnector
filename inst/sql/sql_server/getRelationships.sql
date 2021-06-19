
{@ingredient_concepts_desc != '' & @condition_concepts_desc != ''} ? {
SELECT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)
INNER JOIN @vocabulary.concept_ancestor ca1 ON c1.concept_id = ca1.descendant_concept_id
INNER JOIN @vocabulary.concept_ancestor ca2 ON c2.concept_id = ca2.descendant_concept_id
    {@use_siblings} ? {
    INNER JOIN @vocabulary.concept_ancestor ca3 ON (ca3.ancestor_concept_id = ca2.ancestor_concept_id AND ca2.max_levels_of_separation <= @sibling_lookup_levels)
    WHERE ca3.descendant_concept_id IN (@condition_concepts_desc)
    } : {
    WHERE ca2.ancestor_concept_id  IN (@condition_concepts_desc)
    }
AND ca1.ancestor_concept_id  IN (@ingredient_concepts_desc)
}

{@ingredient_concepts_no_des != '' & @condition_concepts_no_desc != ''} ? {
    {@ingredient_concepts_desc != '' & @condition_concepts_desc != ''} ? {
    UNION
    }
SELECT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)

WHERE cu.concept_id_1 IN (@ingredient_concepts_no_desc)
AND cu.concept_id_2  IN (@condition_concepts_no_desc)
}

{@ingredient_concepts_no_des != '' & @condition_concepts_desc != ''} ? {
    {(@ingredient_concepts_desc != '' & @condition_concepts_desc != '') ||
        (@ingredient_concepts_no_des != '' & @condition_concepts_no_desc != '')} ? {
        UNION
    }
SELECT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)
INNER JOIN @vocabulary.concept_ancestor ca2 ON c2.concept_id = ca2.descendant_concept_id
{@use_siblings} ? {
INNER JOIN @vocabulary.concept_ancestor ca3 ON (ca3.ancestor_concept_id = ca2.ancestor_concept_id AND ca2.max_levels_of_separation <= @sibling_lookup_levels)
WHERE ca3.descendant_concept_id IN (@condition_concepts_desc)
} : {
WHERE ca2.ancestor_concept_id  IN (@condition_concepts_desc)
}
AND cu.concept_id_1 IN (@ingredient_concepts_no_desc)
}

{@ingredient_concepts_des != '' & @condition_concepts_no_desc != ''} ? {
    {(@ingredient_concepts_desc != '' & @condition_concepts_desc != '') ||
        (@ingredient_concepts_no_des != '' & @condition_concepts_no_desc != '') ||
            (@ingredient_concepts_no_des != '' & @condition_concepts_desc != '')} ? {
            UNION
    }
SELECT
    c1.concept_name as concept_name_1,
    c2.concept_name as concept_name_2,
    cu.*
FROM @cem_schema.cem_unified cu

INNER JOIN @vocabulary.concept c1 ON (c1.concept_id = cu.concept_id_1)
INNER JOIN @vocabulary.concept c2 ON (c2.concept_id = cu.concept_id_2)
INNER JOIN @vocabulary.concept_ancestor ca2 ON c2.concept_id = ca2.descendant_concept_id
WHERE ca1.ancestor_concept_id IN (@ingredient_concepts_desc)
AND cu.concept_id_2  IN (@condition_concepts_no_desc)
}
