#! /bin/bash
#SBATCH --job-name=2.1.7_Estimate_Trans_Aboundance_V3_Nopre
#SBATCH --cpus-per-task=8
#SBATCH --mem=8G
#SBATCH --partition=medium
#SBATCH --mail-user=rz40@st-andrews.ac.uk
#SBATCH --mail-type=END,FAIL
#SBATCH --output %x_%j.out
#SBATCH --error %x_%j.err
 
echo "HOSTNAME: $HOSTNAME"
echo "Start Date: $(date)"

#########################################
## StringTie_Estimate_Trans_Aboundance ##
#########################################
bamdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/02_SAM_to_BAM'
Mergeddir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/2.1.7_04_StringTie_Merged_NoPre'
outdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/2.1.7_05_Estimate_Aboundance_NoPre'

ls $bamdir > bamid
cat bamid | while read id
do
    stringtie -e -p 8 -G $Mergeddir/stringtie_merged.gtf -o $outdir/${id%%.*}/${id%%.*}.gtf $bamdir/$id
done

echo "End Date: $(date)"
