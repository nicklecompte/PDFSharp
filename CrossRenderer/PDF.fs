module PDF

type PDFDocument<'TRender> = {
    path : string
    renderer : PDFDocument -> 'TRender
}
