#! /bin/bash
#SBATCH --job-name=2.1.7.Strintie_Assemble_V3_no_predication
#SBATCH --cpus-per-task=8
#SBATCH --mem=16G
#SBATCH --partition=medium
#SBATCH --mail-user=rz40@st-andrews.ac.uk
#SBATCH --mail-type=END,FAIL
#SBATCH --output %x_%j.out
#SBATCH --error %x_%j.err
 
echo "HOSTNAME: $HOSTNAME"
echo "Start Date: $(date)"
 
#########################################
## StringTie_Assemble_Transcripts      ##
#########################################
bamdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/02_SAM_to_BAM'
Refdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/01.v3.genome'
Outdir='/home/rezhang/scratch/private/SwRNAseq_20221201/v3.genome.mapping/03_REUSLT/2.1.7_03_StringTie_Alignment_NoPre'
ls $bamdir > bamid
cat bamid | while read id
do
    stringtie -p 8 -e -G $Refdir/TOC.asm.scaffold.gene.gff3 \
    -o $Outdir/${id%%.*}.gtf \
    -l ${id%%.*} $bamdir/$id
done

echo "End Date: $(date)"
